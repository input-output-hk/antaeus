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

module PlutusScripts.BLS.SchnorrG2 (
    verifyBlsSchnorrG2AssetIdV2
  , verifyBlsSchnorrG2MintWitnessV2
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
  { message = "2b71175d0486006a33f14bc4e1fe711a3d4a3a3549b230013240e8f80e54372f"
  , pubKey = "88370a4b4ddc627613b0396498fb068f1c1ff8f2aa6b946a9fc65f930d24394ddc45042e602094f6a88d49a8a037e781" ++
             "08dce014586ff5ff5744842f382e3917d180c7eb969585748d20ae8c6e07ca786e8da7ea2c7bdef5ae1becebe4da59ad"
  , signature = ("964851eb8823492c8720bf8c515b87043f5bab648000e63cfb6fc6fcdf6709061e0035c315cd23d239866471dea907d9" ++
                 "1568b69297dc8c4360f65d0bd399c2de19781c13bbf3a82ff1fcab8ac9f688ed96d6f2ea9a8ed057e76f0347d858ae22",
                 "2c5a22cb1e2fb77586c0c6908060b38107675a6277b8a61b1d6394a162af6718")
  }

---- BLS Schnorr signature in G2 ----

{-
  * hash = Sha256(A || pk || msg)[..16]
  * pk_deser = G2Decompress(pk)
  * A_deser = G2Decompress(A)
  * r_deser = IntegerFromBytes(r)
  * Check that r_deser * G2Generator = A_deser + c * pk_deser
-}
{-# INLINABLE mkBlsSchnorrG2 #-}
mkBlsSchnorrG2 :: BlsParams -> sc -> Bool
mkBlsSchnorrG2 BlsParams{..} _sc = do
  let
    A = fst signature
    r = snd signature
    c = BI.bls12_381_G2_uncompress $ BI.sliceByteString 0 16
      (BI.sha2_256 (A `BI.appendByteString` pubkey `BI.appendByteString` message)) 0 0
    pkDeser = BI.bls12_381_G2_uncompress pubkey
    ADeser = BI.bls12_381_G2_uncompress A
    rDeser = convertByteStringToInteger r 0 0
  BI.bls12_381_finalVerify (rDeser `BI.bls12_381_G2_scalarMul` g2)
                           ((ADeser `BI.bls12_381_G2_add` c) `BI.bls12_381_G2_scalarMul` pkDeser)
    where
      -- an inefficient workaround for lack of ByteString to Integer interpretation
      -- to be addressed by byteStringToInteger in https://github.com/input-output-hk/plutus/pull/4733
      convertByteStringToInteger :: BuiltinByteString -> Integer -> Integer -> Integer
      convertByteStringToInteger bs i acc
        | i >= lengthOfByteString bs = acc
        | otherwise = convertByteStringToInteger bs (i + 1) (256 * acc + indexByteString bs i) -- (acc + (indexByteString bs i) * 256 ^ i)


verifyBlsSchnorrG2PolicyV2 :: MintingPolicy
verifyBlsSchnorrG2PolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkBlsSchnorrG2

verifyBlsSchnorrG2PolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsSchnorrG2PolicyScriptV2 = policyScript verifyBlsSchnorrG2PolicyV2

verifyBlsSchnorrG2AssetIdV2 :: C.AssetId
verifyBlsSchnorrG2AssetIdV2 = C.AssetId (policyIdV2 verifyBlsSchnorrG2PolicyV2) blsAssetName

verifyBlsSchnorrG2Redeemer :: C.HashableScriptData
verifyBlsSchnorrG2Redeemer = toScriptData redeemerParams

verifyBlsSchnorrG2MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSchnorrG2MintWitnessV2 era =
    (policyIdV2 verifyBlsSchnorrG2PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSchnorrG2PolicyScriptV2) verifyBlsSchnorrG2Redeemer)
