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

module PlutusScripts.BLS.AggregateSigWithMultipleKeys (
    verifyBlsAggregateSigMultiKeyG1AssetIdV2
  , verifyBlsAggregateSigMultiKeyG2MintWitnessV2
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
    { message            :: P.BuiltinByteString
    , pubKeys            :: [P.BuiltinByteString]
    , aggregateSignature :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams = BlsParams
  { message = "e345b7f2c017b16bb335c696bc0cc302f3db897fa25365a2ead1f149d87a97e8"
  , pubKeys = [
      "83718f20d08471565b3a6ca6ea82c1928e8730f87e2afe460b74842f2880facd8e63b8abcdcd7350fe5813a08aa0efed" ++
      "13216b10de1c56dc059c3a8910bd97ae133046ae031d2a53a44e460ab71ebda94bab64ed7478cf1a91b6d3981e32fc95",
      "814f825911bd066855333b74a3cc564d512503ee29ea1ec3bd57a3c07fa5768ad27ea1ddd8047f43fbc9a4ebda897c14" ++
      "06415fefbb8838b8782aa747e2fde7b1813d0f89fad06c8971041c9427abf848503e34e3ca033ba85d50b72ffac4be4a",
      "9974c70513ed5538a8e55f5ce1a0267282b9e8431e25ae566950b2d0793a44a0a3c52110f4d83d694a5296615ee68573" ++
      "098c14d255783a9b1a169d2be1baefbef914a4f830a9099f720063914cc919064d2244582bb9f302eac39c8b195cf3d2",
      "894a3a01d38169a38bea13097cf904dd3ff9dceefb51e8b539725a237ae55a361758be1cdf0e21a7b8db3599adaf2305" ++
      "050f1d8450b924a4b910ff536fc2f7960cd3251c2a457b975d46f7c0f74493cc9b5e8d2fed2e489363e641cc79933d1e",
      "9646da0149ed140e33a99e1ffc5fe9c97c2368ca273544024993cdcb7aa04c0be936e6d4427747e62c4caea4fe1f69e5" ++
      "162fad222e0487f5556524c9d3db74921e1c0f5893f0e26c759e3873e8fd6637e6051f70ef9a3363cf284e8eee67bcf3",
      "b75743fb2f8321ac56cee19aacd7e141a3592b7230992ea84d8800d45ad71924a477f61cf9d4a2783df59dac21cd17e7" ++
      "0e4ce5d526cbe73edc4a10b78fa56a2ef34d2009f2756d2d50188031e026a6a1dadcd5e753f5e7f7276048277d3819f1",
      "873c1e7d525265afa8c037d33874261a90daaa2c6ed5e46ed043ec48a28b7111d0de65800aa72448c1fdb1026ba076bd" ++
      "04193bd2d04e0de63e7a008b8417420eb4920767a1d32f6330ed25bdb4dc7726d989d6cf192db6b32728bb388195ba27",
      "b993f867f9f1f84c3c5c3e5b80013055da7705491c36a80e1201a6a503d7364000c50bc27e03477646874a3074cc4e39" ++
      "0febfea78a2b4d0e40c57d6deaf9fae430a19fcce0c03f43ff8f7e788de0c7b8ce1b69b69d1d026175c8f2730777866d",
      "99836a204576636f34a4663cfa7e02a05cb2d4fd1b582427d199ac3ddac6f087968d2290198aa15e04f6e7e0d070b7dd" ++
      "03607db9c2e4b17709853c30b2f6490261599408fbbc17371de74d0a2a76ff10cd8c9b55461c444bbebc82547bb40c9f",
      "96f8d678f40dd83b2060e14372d0bc43a423fecac44f082afd89cb481b855885ac83fb366516dc74023cc41a0c606be2" ++
      "067ba826ea612f84c9f0e895d02bc04d6c34e201ff8c26cc22cb4c426c53f503d8948eafceb12e2f4b6ad49b4e051690"
      ]
  , aggregateSignature =
      "89d9757c2467dfd987f35c462b7a4adf8e7bfd6fb82edfd42a22f985083f4e6fc45ad2548093fb479b2bd1f48b446ae6"
  }

---- BLS aggregate signature with different keys and same message with public key over G2 ----

{-
-- * hashed_msg = G1HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
-- * pk_deser_i = G1Decompress(pk_i) for i in [1, 10]
-- * ds_scalar = SHA256(pk_1 || .. || pk_10)[..16] (where [..16] represent the first 16 bytes)
-- * aggr_sig_deser = G2Decompress(aggr_sig)
-- * aggr_pk = sum_{i\in[1,10]} ds_scalar^i * pk_deser_i
-- * Check that pairing(aggr_pk, hashed_msg) = pairing(G1Generator, aggr_sig_deser)
-}
{-# INLINABLE mkAggregateSigMultiKeyG1 #-}
mkBlsAggregateSigMultiKeyG2 :: BlsParams -> sc -> Bool
mkBlsAggregateSigMultiKeyG2 BlsParams{..} _sc = do
  let
    hashedMsg = BI.bls12_381_G1_hashToGroup message blsSigBls12381G2XmdSha256SswuRoNul
    pksDeser = BI.bls12_381_G1_uncompress <$> pubKeys
    dsScalar = convertByteStringToInteger $ BI.sliceByteString 0 16
      (BI.sha2_256 $ BI.foldl BI.appendByteString (head pubKeys) pubKeys) 0 0 -- PlutusTx.Foldable has no foldl1
    aggrSigDeser = BI.bls12_381_G2_uncompress aggregateSignature
    aggrPk = calcAggregatedPubkeys dsScalar pksDeser 0 0
  BI.bls12_381_finalVerify (BI.bls12_381_millerLoop aggrPk hashedMsg) (BI.bls12_381_millerLoop g1 aggrSigDeser)
    where
      -- an inefficient workaround for lack of ByteString to Integer interpretation
      -- to be addressed by byteStringToInteger in https://github.com/input-output-hk/plutus/pull/4733
      convertByteStringToInteger :: BuiltinByteString -> Integer -> Integer -> Integer
      convertByteStringToInteger bs i acc
        | i >= lengthOfByteString bs = acc
        | otherwise = convertByteStringToInteger bs (i + 1) (256 * acc + indexByteString bs i) -- (acc + (indexByteString bs i) * 256 ^ i)

      calcAggregatedPubkeys :: Integer -> BI.BuiltinByteString -> Integer -> Integer -> Integer
      calcAggregatedPubkeys dsScalar' pksDeser' i acc
        | i >= length pksDeser' = acc
        | otherwise = calcAggregatedPubkeys dsScalar' pksDeser' (i + 1)
                      (acc + dsScalar' ^ (i + 1) `BI.bls12_381_G1_scalarMul` pksDeser' !! i)

verifyBlsAggregateSigMultiKeyG2PolicyV2 :: MintingPolicy
verifyBlsAggregateSigMultiKeyG2PolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkBlsAggregateSigMultiKeyG2

verifyBlsAggregateSigMultiKeyG2PolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsAggregateSigMultiKeyG2PolicyScriptV2 = policyScript verifyBlsAggregateSigMultiKeyG2PolicyV2

verifyBlsAggregateSigMultiKeyG1AssetIdV2 :: C.AssetId
verifyBlsAggregateSigMultiKeyG1AssetIdV2 = C.AssetId (policyIdV2 verifyBlsAggregateSigMultiKeyG2PolicyV2) blsAssetName

verifyBlsAggregateSigMultiKeyG2Redeemer :: C.HashableScriptData
verifyBlsAggregateSigMultiKeyG2Redeemer = toScriptData redeemerParams

verifyBlsAggregateSigMultiKeyG2MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsAggregateSigMultiKeyG2MintWitnessV2 era =
    (policyIdV2 verifyBlsAggregateSigG2PolicyV2,
     mintScriptWitness era plutusL2
       (Left verifyBlsAggregateSigMultiKeyG2PolicyScriptV2) verifyBlsAggregateSigMultiKeyG2Redeemer)
