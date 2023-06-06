{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use underscore" #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusScripts.BLS.SimpleSignAndVerify (
    verifyBlsSimpleAssetIdV2
  , verifyBlsSimpleMintWitnessV2
  ) where

import Cardano.Api qualified as C
import Data.ByteString as BS hiding (foldl, map)
import Data.Word (Word8)
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import OldPlutus.Scripts (MintingPolicy, mkMintingPolicyScript)
import PlutusCore (DefaultFun, DefaultUni)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.BLS.Common
import PlutusScripts.Helpers (bytesFromHex, mintScriptWitness, plutusL1, plutusL2, policyIdV1, policyIdV2, policyScript,
                              toScriptData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P
import UntypedPlutusCore qualified as UPLC

data BlsParams = BlsParams
    { privKey :: Integer -- 32 bit private key
    , message :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams = BlsParams
  { -- sha256 hash of the phrase "I am a secret key" as an Integer
    privKey = 50166937291276222007610100461546392414157570314060957244808461481762532157524 :: Integer
  , message  = BI.toBuiltin $ bytesFromHex "I am a message"
  }

-- BLS 12 381 simple verify with private key minting policy --

{-# INLINABLE mkVerifyBlsSimplePolicy #-}
mkVerifyBlsSimplePolicy :: BlsParams -> sc -> Bool
mkVerifyBlsSimplePolicy BlsParams{..} _sc = do
  let
    -- calculate public key
    pubKey = BI.bls12_381_G1_scalarMul privKey g1

    -- Hash this msg to the G2
    msgToG2 = BI.bls12_381_G2_hashToGroup message BI.emptyByteString

    -- Create signature artifact in G2 with private key
    sigma = BI.bls12_381_G2_scalarMul privKey msgToG2

  -- verify the msg with signature sigma with the check e(g1,sigma)=e(pub,msgToG2)
  BI.bls12_381_finalVerify (BI.bls12_381_millerLoop g1 sigma) (BI.bls12_381_millerLoop pubKey msgToG2)

verifyBlsSimplePolicyV2 :: MintingPolicy
verifyBlsSimplePolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifyBlsSimplePolicy

verifyBlsSimplePolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsSimplePolicyScriptV2 = policyScript verifyBlsSimplePolicyV2

verifyBlsSimpleAssetIdV2 :: C.AssetId
verifyBlsSimpleAssetIdV2 = C.AssetId (policyIdV2 verifyBlsSimplePolicyV2) blsAssetName

verifyBlsSimpleRedeemer :: C.HashableScriptData
verifyBlsSimpleRedeemer = toScriptData redeemerParams

verifyBlsSimpleMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSimpleMintWitnessV2 era =
    (policyIdV2 verifyBlsSimplePolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSimplePolicyScriptV2) verifyBlsSimpleRedeemer)
