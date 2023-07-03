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

module PlutusScripts.BLS.VerifyOverG1 (
    verifyBlsSigG1AssetIdV2
  , verifyBlsSigG1MintWitnessV2
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
    { message   :: P.BuiltinByteString
    , pubKey    :: P.BuiltinByteString
    , signature :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams = BlsParams
  { message = BI.toBuiltin $ bytesFromHex "3e00ef2f895f40d67f5bb8e81f09a5a12c840ec3ce9a7f3b181be188ef711a1e"
  , pubKey = BI.toBuiltin $ bytesFromHex ("aa04a34d4db073e41505ebb84eee16c0094fde9fa22ec974" <>
                                           "adb36e5b3df5b2608639f091bff99b5f090b3608c3990173")
  , signature = BI.toBuiltin $ bytesFromHex
                  ("808ccec5435a63ae01e10d81be2707ab55cd0dfc235dfdf9f70ad32799e42510d67c9f61d98a6578a96a76cf6f4c105d" <>
                  "09262ec1d86b06515360b290e7d52d347e48438de2ea2233f3c72a0c2221ed2da5e115367bca7a2712165032340e0b29")
  }

---- BLS signature with the public key over G1 ----
{-
  * pk_deser = G1Decompress(pk)
  * sig_deser = G2Decompress(sig)
  * hashed_msg = G2HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
  * Check that pairing(pk_deser, hashed_msg) = pairing(G1Generator, sig_deser)
-}
{-# INLINABLE mkVerifySigG1 #-}
mkVerifySigG1 ::
     BuiltinByteString
  -> BuiltinBLS12_381_G1_Element
  -> BlsParams
  -> sc
  -> Bool
mkVerifySigG1 dst g1Gen BlsParams{..} _sc = do
  let
    pkDeser = BI.bls12_381_G1_uncompress pubKey
    sigDeser = BI.bls12_381_G2_uncompress signature
    hashedMsg = BI.bls12_381_G2_hashToGroup message dst

  BI.bls12_381_finalVerify (BI.bls12_381_millerLoop pkDeser hashedMsg) (BI.bls12_381_millerLoop g1Gen sigDeser)

verifyBlsSigG1PolicyV2 :: MintingPolicy
verifyBlsSigG1PolicyV2 = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode blsSigBls12381G2XmdSha256SswuRoNul
    `PlutusTx.applyCode` PlutusTx.liftCode g1Generator
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifySigG1

verifyBlsSigG1PolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsSigG1PolicyScriptV2 = policyScript verifyBlsSigG1PolicyV2

verifyBlsSigG1AssetIdV2 :: C.AssetId
verifyBlsSigG1AssetIdV2 = C.AssetId (policyIdV2 verifyBlsSigG1PolicyV2) blsAssetName

verifyBlsSigG1Redeemer :: C.HashableScriptData
verifyBlsSigG1Redeemer = toScriptData redeemerParams

verifyBlsSigG1MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSigG1MintWitnessV2 era =
    (policyIdV2 verifyBlsSigG1PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSigG1PolicyScriptV2) verifyBlsSigG1Redeemer)
