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

module PlutusScripts.BLS.VerifyOverG2 (
    verifyBlsSigG2AssetIdV2
  , verifyBlsSigG2MintWitnessV2
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
  { message = "5032ec38bbc5da98ee0c6f568b872a65a08abf251deb21bb4b56e5d8821e68aa"
  , pubKey = "b4953c4ba10c4d4196f90169e76faf154c260ed73fc77bb65dc3be31e0cec614a7287cda94195343676c2c57494f0e651527e6504c98408e599a4eb96f7c5a8cfb85d2fdc772f28504580084ef559b9b623bc84ce30562ed320f6b7f65245ad4"
  , signature = "a9d4de7b0b2805fe52bccb86415ef7b8ffecb313c3c254044dfc1bdc531d3eae999d87717822a052692140774bd7245c"
  }

---- BLS signature with the public key over G2 ----

{-
  * pk_deser = G2Decompress(pk)
  * sig_deser = G1Decompress(sig)
  * hashed_msg = G1HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
  * Check that pairing(pk_deser, hashed_msg) = pairing(G1Generator, sig_deser)
-}
{-# INLINABLE mkVerifySigG2 #-}
mkVerifySigG2 :: BlsParams -> sc -> Bool
mkVerifySigG2 BlsParams{..} _sc = do
  let
    pkDeser = BI.bls12_381_G2_uncompress pubKey
    sigDeser = BI.bls12_381_G1_uncompress signature
    hashedMsg = BI.bls12_381_G1_hashToGroup message blsSigBls12381G2XmdSha256SswuRoNul

  BI.bls12_381_finalVerify (BI.bls12_381_millerLoop pkDeser hashedMsg) (BI.bls12_381_millerLoop g1 sigDeser)

verifyBlsSigG2PolicyV2 :: MintingPolicy
verifyBlsSigG2PolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifySigG2

verifyBlsSigG2PolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsSigG2PolicyScriptV2 = policyScript verifyBlsSigG2PolicyV2

verifyBlsSigG2AssetIdV2 :: C.AssetId
verifyBlsSigG2AssetIdV2 = C.AssetId (policyIdV2 verifyBlsSigG2PolicyV2) blsAssetName

verifyBlsSigG2Redeemer :: C.HashableScriptData
verifyBlsSigG2Redeemer = toScriptData redeemerParams

verifyBlsSigG2MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSigG2MintWitnessV2 era =
    (policyIdV2 verifyBlsSigG2PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSigG2PolicyScriptV2) verifyBlsSigG2Redeemer)
