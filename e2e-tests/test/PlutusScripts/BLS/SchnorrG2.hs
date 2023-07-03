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
  { message = toBuiltin $ bytesFromHex "2b71175d0486006a33f14bc4e1fe711a3d4a3a3549b230013240e8f80e54372f"
  , pubKey = toBuiltin $ bytesFromHex
              ("88370a4b4ddc627613b0396498fb068f1c1ff8f2aa6b946a9fc65f930d24394ddc45042e602094f6a88d49a8a037e781" <>
              "08dce014586ff5ff5744842f382e3917d180c7eb969585748d20ae8c6e07ca786e8da7ea2c7bdef5ae1becebe4da59ad")
  , signature =
          (toBuiltin $ bytesFromHex
            ("964851eb8823492c8720bf8c515b87043f5bab648000e63cfb6fc6fcdf6709061e0035c315cd23d239866471dea907d9" <>
             "1568b69297dc8c4360f65d0bd399c2de19781c13bbf3a82ff1fcab8ac9f688ed96d6f2ea9a8ed057e76f0347d858ae22"),
           toBuiltin $ bytesFromHex "2c5a22cb1e2fb77586c0c6908060b38107675a6277b8a61b1d6394a162af6718")
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
mkBlsSchnorrG2 ::
     BuiltinByteString
  -> BuiltinByteString
  -> BlsParams
  -> sc
  -> Bool
mkBlsSchnorrG2 bs16Null g2Gen BlsParams{..} _sc = do
  let
    a = Tx.fst signature
    r = Tx.snd signature
    c = byteStringToInteger (Tx.sliceByteString 0 16
      (Tx.sha2_256 (a `Tx.appendByteString` pubKey `Tx.appendByteString` message)) `Tx.appendByteString` bs16Null)
    pkDeser = Tx.bls12_381_G2_uncompress pubKey
    aDeser = Tx.bls12_381_G2_uncompress a
    rDeser = byteStringToInteger r
  (rDeser `Tx.bls12_381_G2_scalarMul` g2Gen) `Tx.bls12_381_G2_equals`
    (aDeser `Tx.bls12_381_G2_add` (c `Tx.bls12_381_G2_scalarMul` pkDeser))
    where
      -- a (probably inefficient) workaround for lack of ByteString to Integer interpretation
      -- to be addressed by byteStringToInteger in https://github.com/input-output-hk/plutus/pull/4733
      byteStringToInteger  :: BuiltinByteString -> Integer
      byteStringToInteger b =
        go 0
          where len = Tx.lengthOfByteString b
                go i =
                    if i >= len
                    then 0
                    else (Tx.indexByteString b i) + 256 * (go (i Tx.+ 1))

verifyBlsSchnorrG2PolicyV2 :: MintingPolicy
verifyBlsSchnorrG2PolicyV2 = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode byteString16Null
    `PlutusTx.applyCode` PlutusTx.liftCode g2Generator
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
