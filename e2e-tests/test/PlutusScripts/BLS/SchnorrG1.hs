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

module PlutusScripts.BLS.SchnorrG1 (
    verifyBlsSchnorrG1AssetIdV2
  , verifyBlsSchnorrG1MintWitnessV2
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
    , signature :: (P.BuiltinByteString, P.BuiltinByteString)
    }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams = BlsParams
  { message = "0558db9aff738e5421439601e7f30e88b74f43b80c1d172b5d371ce0dc05c912"
  , pubKey = "b91cacee903a53383c504e9e9a39e57d1eaa6403d5d38fc9496e5007d54ca92d106d1059f09461972aa98514d07000ae"
  , signature = ("8477e8491acc1cfbcf675acf7cf6b92e027cad7dd604a0e8205703aa2cc590066c1746f89e10d492d0230e6620c29726",
                 "4e908280c0100cfe53501171ffa93528b9e2bb551d1025decb4a5b416a0aee53")
  }

---- BLS Schnorr signature in G1 ----

{-
  * c = Sha256(A || pk || msg)[..16]
  * pk_deser = G1Decompress(pk)
  * A_deser = G1Decompress(A)
  * r_deser = IntegerFromBytes(r)
  * Check that r_deser * G1Generator = A_deser + c * pk_deser
-}
{-# INLINABLE mkBlsSchnorrG1 #-}
mkBlsSchnorrG1 :: BlsParams -> sc -> Bool
mkBlsSchnorrG1 BlsParams{..} _sc = do
  let
    A = fst signature
    r = snd signature
    c = BI.bls12_381_G1_uncompress $ BI.sliceByteString 0 16
      (BI.sha2_256 (A `BI.appendByteString` pubkey `BI.appendByteString` message)) 0 0
    pkDeser = BI.bls12_381_G1_uncompress pubkey
    ADeser = BI.bls12_381_G1_uncompress A
    rDeser = convertByteStringToInteger r 0 0
  BI.bls12_381_finalVerify (rDeser `BI.bls12_381_G1_scalarMul` g1)
                           ((ADeser `BI.bls12_381_G1_add` c) `BI.bls12_381_G1_scalarMul` pkDeser)
    where
      -- an inefficient workaround for lack of ByteString to Integer interpretation
      -- to be addressed by byteStringToInteger in https://github.com/input-output-hk/plutus/pull/4733
      convertByteStringToInteger :: BuiltinByteString -> Integer -> Integer -> Integer
      convertByteStringToInteger bs i acc
        | i >= lengthOfByteString bs = acc
        | otherwise = convertByteStringToInteger bs (i + 1) (256 * acc + indexByteString bs i) -- (acc + (indexByteString bs i) * 256 ^ i)


verifyBlsSchnorrG1PolicyV2 :: MintingPolicy
verifyBlsSchnorrG1PolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkBlsSchnorrG1

verifyBlsSchnorrG1PolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsSchnorrG1PolicyScriptV2 = policyScript verifyBlsSchnorrG1PolicyV2

verifyBlsSchnorrG1AssetIdV2 :: C.AssetId
verifyBlsSchnorrG1AssetIdV2 = C.AssetId (policyIdV2 verifyBlsSchnorrG1PolicyV2) blsAssetName

verifyBlsSchnorrG1Redeemer :: C.HashableScriptData
verifyBlsSchnorrG1Redeemer = toScriptData redeemerParams

verifyBlsSchnorrG1MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSchnorrG1MintWitnessV2 era =
    (policyIdV2 verifyBlsSchnorrG1PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSchnorrG1PolicyScriptV2) verifyBlsSchnorrG1Redeemer)
