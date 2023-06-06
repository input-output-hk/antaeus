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

module PlutusScripts.BLS (
    verifyBlsSimpleAssetIdV2
  , verifyBlsSimpleMintWitnessV2
  , verifyBlsVrfAssetIdV2
  , verifyBlsVrfMintWitnessV2
  , verifyBlsGroth16AssetIdV2
  , verifyBlsGroth16MintWitnessV2
  ) where

import Cardano.Api qualified as C
import Data.ByteString as BS hiding (foldl, map)
import Data.Word (Word8)
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import OldPlutus.Scripts (MintingPolicy, mkMintingPolicyScript)
import PlutusCore (DefaultFun, DefaultUni)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.Helpers (bytesFromHex, mintScriptWitness, plutusL1, plutusL2, policyIdV1, policyIdV2, policyScript,
                              toScriptData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P
import UntypedPlutusCore qualified as UPLC

data BlsParamsWithPrivKey = BlsParamsWithPrivKey
    { privKey     :: Integer -- 32 bit private key
    , privMessage :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BlsParamsWithPrivKey

data BlsParamsWithPubKey = BlsParamsWithPubKey
    { pubMessage :: P.BuiltinByteString
    , pubKey     :: P.BuiltinByteString
    , pubSig     :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BlsParamsWithPubKey

data BlsParamsWithAggregateSignature = BlsParamsWithAggregateSignature
    { aggregateSigMessages :: [P.BuiltinByteString]
    , aggregateSigPubKey   :: P.BuiltinByteString
    , aggregateSignature   :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BlsParamsWithAggregateSignature

blsAssetName :: C.AssetName
blsAssetName = C.AssetName "BLS"

verifyBlsParamsWithPrivKey :: BlsParamsWithPrivKey
verifyBlsParamsWithPrivKey = BlsParamsWithPrivKey
  { -- sha256 hash of the phrase "I am a secret key" as an Integer
    privKey = 50166937291276222007610100461546392414157570314060957244808461481762532157524 :: Integer
  , privMessage  = BI.toBuiltin $ bytesFromHex "I am a message"
  }

blsSigBls12381G2XmdSha256SswuRoNul :: P.BuiltinByteString
blsSigBls12381G2XmdSha256SswuRoNul = "424c535f5349475f424c53313233383147325f584d443a5348412d3235365f535357555f524f5f4e554c5f" -- ascci "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"

-- G1 and G2 generators
{-# INLINABLE g1 #-}
g1 = BI.bls12_381_G1_uncompress $ toBuiltin $ pack [151, 241, 211, 167, 49, 151, 215, 148, 38, 149, 99, 140, 79, 169, 172, 15, 195, 104, 140, 79, 151, 116, 185, 5, 161,78, 58, 63, 23, 27, 172, 88, 108, 85, 232, 63, 249, 122, 26, 239, 251, 58, 240, 10, 219, 34, 198, 187]
{-# INLINABLE g2 #-}
g2 = BI.bls12_381_G2_uncompress $ toBuiltin $ pack [147, 224, 43, 96, 82, 113, 159, 96, 125, 172, 211, 160, 136, 39, 79, 101, 89, 107, 208, 208, 153, 32, 182,  26, 181, 218, 97, 187, 220, 127, 80, 73, 51, 76, 241, 18, 19, 148, 93, 87, 229, 172, 125, 5, 93, 4, 43, 126,  2, 74, 162, 178, 240, 143, 10, 145, 38, 8, 5, 39, 45, 197, 16, 81, 198, 228, 122, 212, 250, 64, 59, 2, 180,  81, 11, 100, 122, 227, 209, 119, 11, 172, 3, 38, 168, 5, 187, 239, 212, 128, 86, 200, 193, 33, 189, 184]

-- BLS 12 381 simple verify with private key minting policy --

{-# INLINABLE mkVerifyBlsSimplePolicy #-}
mkVerifyBlsSimplePolicy :: BlsParamsWithPrivKey -> sc -> Bool
mkVerifyBlsSimplePolicy BlsParamsWithPrivKey{..} _sc = do
  let
    -- calculate public key
    pubKey = BI.bls12_381_G1_scalarMul privKey g1

    -- Hash this msg to the G2
    msgToG2 = BI.bls12_381_G2_hashToGroup privMessage BI.emptyByteString

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
verifyBlsSimpleRedeemer = toScriptData verifyBlsParamsWithPrivKey

verifyBlsSimpleMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSimpleMintWitnessV2 era =
    (policyIdV2 verifyBlsSimplePolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSimplePolicyScriptV2) verifyBlsSimpleRedeemer)


---- Verify BLS VRF ----

{- | A basic VRF using G2 (using G1 would be cheaper)
  for reference see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-15#name-elliptic-curve-vrf-ecvrf
  and more readable and mathematical in nature see https://eprint.iacr.org/2017/099.pdf.
-}

{-# INLINABLE mkVerifyBlsVrfPolicy #-}
mkVerifyBlsVrfPolicy :: BlsParamsWithPrivKey -> sc -> Bool
mkVerifyBlsVrfPolicy BlsParamsWithPrivKey{..} _sc = do
  let
    -- calculate public key
    pub = BI.bls12_381_G2_scalarMul privKey g2

    -- hash this msg to G2
    h = BI.bls12_381_G2_hashToGroup (toBuiltin privMessage) BI.emptyByteString

    -- define first element of the proof of correct VRF
    gamma = BI.bls12_381_G2_scalarMul privKey h

    -- for this signed hash with preimage alpha, define a ephemeral interger (for each signature take a new one). Random 32 byte int
    k = 108204667002115086588596846168569722098834602153875763359385781912495445631691 :: Integer

    -- define second element of the proof of correct VRF
    -- the paper notes that this can actually be truncated to 128 bits without loss of the 128 bits security.
    -- truncating this will allow for smaller proof sizes.
    c = sha2_256 . mconcat $ BI.bls12_381_G2_compress <$> [g2, h, pub, gamma, BI.bls12_381_G2_scalarMul k g2, BI.bls12_381_G2_scalarMul k h]

    -- define the third and last element of a proof of correct VRF
    s = (k - (os2ip c) * priv) `modulo` 52435875175126190479447740508185965837690552500527637822603658699938581184513

    -- Define the cofactor of G2
    f = 305502333931268344200999753193121504214466019254188142667664032982267604182971884026507427359259977847832272839041692990889188039904403802465579155252111 :: Integer

    -- create our VRF hash output
    beta = sha2_256 . BI.bls12_381_G2_compress $ BI.bls12_381_G2_scalarMul f gamma

    -- The proof of that the VRF hash of input alpha under our priv key is beta
    pi = (gamma, c, s)

    -- To verify a VRF hash given an
    --        input alpha
    --        output beta
    --        proof pi (gamma, c, s)
    --        pubkey pub
    -- do the following calculation
    u  = BI.bls12_381_G2_add (BI.bls12_381_G2_scalarMul (os2ip c) pub) (BI.bls12_381_G2_scalarMul s g2)
    h' = BI.bls12_381_G2_hashToGroup (toBuiltin privMessage) BI.emptyByteString
    v  = BI.bls12_381_G2_add (BI.bls12_381_G2_scalarMul (os2ip c) gamma) (BI.bls12_381_G2_scalarMul s h')

  -- and check
  c == (sha2_256 . mconcat $ BI.bls12_381_G2_compress <$> [g2,h',pub,gamma,u,v])

  where
    os2ip :: BI.BuiltinByteString -> Integer
    os2ip bs
      | bs == emptyByteString = error ()
      | otherwise             = go bs
      where
        len xs = lengthOfByteString xs - 1
        intAtLastByte xs = indexByteString xs $ len xs
        stripLastByte xs = takeByteString (len xs) xs
        go xs
          | xs == emptyByteString = 0
          | otherwise             = intAtLastByte xs + 256 * go (stripLastByte xs)

verifyBlsVrfPolicyV2 :: MintingPolicy
verifyBlsVrfPolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifyBlsVrfPolicy

verifyBlsVrfPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsVrfPolicyScriptV2 = policyScript verifyBlsVrfPolicyV2

verifyBlsVrfAssetIdV2 :: C.AssetId
verifyBlsVrfAssetIdV2 = C.AssetId (policyIdV2 verifyBlsVrfPolicyV2) blsAssetName

verifyBlsVrfRedeemer :: C.HashableScriptData
verifyBlsVrfRedeemer = toScriptData verifyBlsParamsWithPrivKey

verifyBlsVrfMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsVrfMintWitnessV2 era =
    (policyIdV2 verifyBlsVrfPolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsVrfPolicyScriptV2) verifyBlsVrfRedeemer)

---- Groth16 zk-SNARK ----

{- | An example of the on-chain computation required for verification of a Groth16
 proof.  The data here is derived from
 https://github.com/achimcc/groth16-example/blob/main/src/lib.rs -}

type UProg   = UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()

-- Wrappers for compressed group elements for slightly better type safety
newtype CompressedG1Element = CompressedG1Element { compressedG1 :: BI.BuiltinByteString }
    deriving newtype (PlutusTx.Lift DefaultUni)

mkG1Element :: [Word8] -> CompressedG1Element
mkG1Element = CompressedG1Element . toBuiltin . BS.pack

newtype CompressedG2Element = CompressedG2Element { compressedG2 :: BI.BuiltinByteString }
    deriving newtype (PlutusTx.Lift DefaultUni)

mkG2Element :: [Word8] -> CompressedG2Element
mkG2Element = CompressedG2Element . toBuiltin . BS.pack

groth16Scalar :: Integer
groth16Scalar = 0x1884d0cbcc5947434e46d19b3e904e18a8ee8d0d39ce9d315f3b00e338c8f618

-- Lots of group elements for input to the computation

alpha :: CompressedG1Element
alpha = mkG1Element [ 0xb7, 0x1d, 0xb1, 0xfa, 0x5f, 0x41, 0x36, 0x2e
                    , 0x93, 0x02, 0x5b, 0x35, 0x56, 0xd7, 0x6e, 0xad
                    , 0x12, 0x25, 0xcf, 0x59, 0x0d, 0x1c, 0xdb, 0x9e
                    , 0x38, 0x2a, 0x1f, 0xeb, 0xb7, 0x96, 0x3d, 0xcd
                    , 0x24, 0xa5, 0x1e, 0x18, 0xdf, 0x04, 0xab, 0x22
                    , 0x1b, 0xec, 0xaf, 0x29, 0x16, 0x9f, 0xaf, 0x25 ]

beta :: CompressedG2Element
beta = mkG2Element [ 0xb3, 0xa2, 0x6b, 0x0b, 0x47, 0x12, 0xe7, 0x8d
                   , 0x5d, 0x71, 0x78, 0x6d, 0x96, 0x13, 0x2a, 0x7c
                   , 0x58, 0x50, 0x23, 0xa3, 0x66, 0x32, 0xca, 0xda
                   , 0x44, 0x17, 0x1a, 0xc3, 0xf4, 0x5d, 0xb5, 0x24
                   , 0xc3, 0xf6, 0x57, 0x0c, 0x8a, 0x3f, 0x7d, 0xec
                   , 0x35, 0xae, 0x1a, 0xc3, 0x30, 0x9b, 0x05, 0xdd
                   , 0x0b, 0x30, 0x6d, 0xb4, 0xf7, 0x4f, 0xd9, 0xec
                   , 0x42, 0x1c, 0xa7, 0x0c, 0x54, 0x42, 0x5d, 0x92
                   , 0x2e, 0xac, 0x4c, 0x40, 0x3b, 0x00, 0xdb, 0x91
                   , 0x6f, 0xde, 0xdf, 0x06, 0x5b, 0xdc, 0xe0, 0x0e
                   , 0xce, 0x17, 0xb9, 0x7a, 0x4e, 0x97, 0x17, 0x3e
                   , 0x4d, 0x59, 0x89, 0x81, 0x8e, 0xdf, 0xaa, 0x4c ]

gamma :: CompressedG2Element
gamma = mkG2Element [ 0xb5, 0xac, 0xb8, 0x00, 0xcd, 0x49, 0xed, 0x8c
                    , 0xbd, 0xdb, 0xf4, 0x91, 0xa1, 0xfc, 0xf8, 0xab
                    , 0xfc, 0x93, 0xf0, 0x9d, 0x38, 0xbb, 0xb2, 0xec
                    , 0xb6, 0xb0, 0x8e, 0x23, 0xa4, 0x64, 0x2c, 0xe5
                    , 0x9c, 0x9b, 0x03, 0x86, 0x53, 0x9a, 0xc3, 0xce
                    , 0xcd, 0xfb, 0x66, 0xa9, 0xf0, 0x27, 0xfc, 0x21
                    , 0x0f, 0x25, 0x95, 0x10, 0x75, 0x64, 0x44, 0xbc
                    , 0x5e, 0xef, 0x65, 0x4f, 0x4d, 0x06, 0x12, 0xb5
                    , 0xd6, 0x37, 0x5f, 0x95, 0x26, 0xb1, 0xb9, 0x66
                    , 0xce, 0x53, 0xb8, 0xf1, 0x25, 0x94, 0xe1, 0xb3
                    , 0x99, 0xd0, 0x82, 0x31, 0xcf, 0xe6, 0xc2, 0x69
                    , 0xa4, 0x4a, 0xa8, 0xd5, 0x87, 0xf2, 0x36, 0x9d ]

delta :: CompressedG2Element
delta = mkG2Element [ 0xb3, 0xaa, 0x79, 0x7b, 0xaf, 0xa3, 0x9a, 0x48
                    , 0xf6, 0xf8, 0x7c, 0x24, 0x83, 0xc8, 0x94, 0xc2
                    , 0x81, 0xc8, 0x07, 0x82, 0x1c, 0x47, 0x30, 0x1f
                    , 0xfb, 0x75, 0x5a, 0xcf, 0xcf, 0xd2, 0x2c, 0x23
                    , 0x23, 0xce, 0xdf, 0x63, 0x49, 0xc7, 0xfe, 0xdd
                    , 0x32, 0x00, 0xa4, 0xae, 0x55, 0x86, 0x31, 0xe5
                    , 0x01, 0xd2, 0x99, 0xeb, 0x93, 0x13, 0x5c, 0x07
                    , 0xcf, 0x69, 0x4c, 0xa1, 0x18, 0xd1, 0xb3, 0x86
                    , 0x49, 0x05, 0x29, 0xc6, 0x0f, 0x57, 0x93, 0x5c
                    , 0xef, 0xa8, 0x9f, 0xca, 0xfa, 0x13, 0xa8, 0x3f
                    , 0x84, 0x20, 0x7b, 0x76, 0xfe, 0x07, 0x8d, 0xc8
                    , 0x59, 0xd4, 0x02, 0x74, 0x3d, 0x46, 0x8c, 0x15 ]

gamma_abc_1 :: CompressedG1Element
gamma_abc_1 = mkG1Element [ 0xb7, 0xf6, 0xd0, 0x6d, 0xd3, 0xe5, 0x24, 0x6e
                          , 0xf6, 0xb5, 0x1b, 0x07, 0x5c, 0x30, 0xb6, 0x8f
                          , 0xd4, 0x90, 0xfb, 0xf8, 0x5e, 0x02, 0x05, 0xf7
                          , 0x9f, 0xa0, 0x4d, 0x81, 0x13, 0x31, 0x92, 0x13
                          , 0x94, 0x63, 0xb5, 0xe8, 0xef, 0xb2, 0x2c, 0x39
                          , 0xef, 0x3d, 0xd1, 0xc5, 0x09, 0x20, 0x15, 0xb8 ]

gamma_abc_2 :: CompressedG1Element
gamma_abc_2 = mkG1Element [ 0xa2, 0xe6, 0x37, 0xdb, 0xff, 0x52, 0xa1, 0xe4
                          , 0xa8, 0xc5, 0xd9, 0x85, 0xb3, 0x41, 0x1f, 0xc5
                          , 0xfd, 0x44, 0xaf, 0x60, 0x7e, 0x42, 0x92, 0x3e
                          , 0xab, 0xb4, 0x7a, 0xd8, 0x76, 0xe1, 0xf0, 0x2b
                          , 0x5b, 0xe0, 0x34, 0xad, 0xaf, 0x73, 0x95, 0x2a
                          , 0xe8, 0xaf, 0xfe, 0xe5, 0xf5, 0x18, 0x41, 0xde ]

a :: CompressedG1Element
a = mkG1Element [ 0xa0, 0x5b, 0xe5, 0x0f, 0xab, 0x57, 0x95, 0xbb
                , 0x87, 0x84, 0x39, 0x3a, 0x50, 0x45, 0xf9, 0x87
                , 0x47, 0x17, 0x3a, 0xd2, 0x87, 0xf5, 0x5e, 0x21
                , 0x34, 0x71, 0xbd, 0x55, 0x97, 0x45, 0x55, 0x14
                , 0x52, 0x45, 0x3c, 0x4c, 0x3a, 0x39, 0xe7, 0xc8
                , 0x83, 0x10, 0x84, 0x9f, 0x3c, 0x7a, 0x1f, 0xc3 ]

b :: CompressedG2Element
b = mkG2Element [ 0xad, 0x63, 0x48, 0xb6, 0xb7, 0xb3, 0x4c, 0x86
                , 0xbf, 0x37, 0xa7, 0x48, 0xcd, 0x2d, 0x82, 0xa2
                , 0x50, 0xdf, 0xc6, 0x48, 0x46, 0x75, 0x66, 0x88
                , 0x25, 0xa1, 0x6f, 0x7d, 0xa6, 0xa0, 0x4d, 0x34
                , 0x24, 0x11, 0x3e, 0x32, 0x5c, 0xe7, 0x34, 0xec
                , 0x44, 0x95, 0x60, 0x82, 0xc0, 0xa0, 0x6e, 0x5f
                , 0x18, 0x68, 0xe1, 0xf1, 0xa6, 0xe5, 0x59, 0xb9
                , 0xfe, 0x81, 0xf1, 0xa9, 0x01, 0xf8, 0xa6, 0x34
                , 0x1b, 0x30, 0x1c, 0x45, 0xb2, 0x5d, 0x30, 0x80
                , 0xfb, 0xc5, 0x03, 0x93, 0x53, 0xd8, 0xf7, 0x1b
                , 0x55, 0x0b, 0x27, 0x4e, 0xc4, 0xc0, 0x7c, 0x70
                , 0xcd, 0x11, 0x53, 0x56, 0x2c, 0x31, 0x4c, 0x97 ]

c :: CompressedG1Element
c = mkG1Element [ 0xb5, 0x69, 0xcc, 0x49, 0x1b, 0x4d, 0xf0, 0x35
                , 0xcb, 0xf4, 0x9e, 0x95, 0x1f, 0xd4, 0xfe, 0x30
                , 0xaa, 0x82, 0x36, 0xb0, 0xe2, 0xaf, 0x68, 0xf4
                , 0xc1, 0x59, 0x2c, 0xd4, 0x0d, 0xeb, 0xeb, 0x71
                , 0x8a, 0xf3, 0x36, 0x39, 0xdb, 0x6b, 0xc1, 0xe2
                , 0xda, 0x9d, 0x98, 0xe5, 0x53, 0xe5, 0xea, 0xed ]

{-# INLINABLE mkVerifyBlsGroth16Policy #-}
mkVerifyBlsGroth16Policy
    :: BI.BuiltinByteString  -- G1
    -> BI.BuiltinByteString  -- G2
    -> BI.BuiltinByteString  -- G2
    -> BI.BuiltinByteString  -- G2
    -> BI.BuiltinByteString  -- G1
    -> BI.BuiltinByteString  -- G1
    -> BI.BuiltinByteString  -- G1
    -> BI.BuiltinByteString  -- G2
    -> BI.BuiltinByteString  -- G1
    -> Integer
    -> sc
    -> Bool
mkVerifyBlsGroth16Policy
    (BI.bls12_381_G1_uncompress -> alpha')
    (BI.bls12_381_G2_uncompress -> beta')
    (BI.bls12_381_G2_uncompress -> gamma')
    (BI.bls12_381_G2_uncompress -> delta')
    (BI.bls12_381_G1_uncompress -> abc1')
    (BI.bls12_381_G1_uncompress -> abc2')
    (BI.bls12_381_G1_uncompress -> a')
    (BI.bls12_381_G2_uncompress -> b')
    (BI.bls12_381_G1_uncompress -> c')
    s =
        let l1 = BI.bls12_381_millerLoop a' b'
            l2 = BI.bls12_381_millerLoop alpha' beta'
            l3 = BI.bls12_381_millerLoop c' delta'
            p  = BI.bls12_381_G1_add  abc1' (BI.bls12_381_G1_scalarMul s abc2')
            l4 = BI.bls12_381_millerLoop p gamma'
            y  = BI.bls12_381_mulMlResult l2 (BI.bls12_381_mulMlResult l3 l4)
        in BI.bls12_381_finalVerify l1 y

{- | Make a UPLC script applying groth16Verify to the inputs.  Passing the
 newtype inputs increases the size and CPU cost slightly, so we unwrap them
 first.  This should return `True`. -}
verifyBlsGroth16PolicyV2 :: MintingPolicy
verifyBlsGroth16PolicyV2 = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . mkVerifyBlsGroth16Policy ||])
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g1 alpha)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g2 beta)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g2 gamma)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g2 delta)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g1 gamma_abc_1)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g1 gamma_abc_2)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g1 a)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g2 b)
      `PlutusTx.applyCode` (PlutusTx.liftCode $ g1 c)
      --`PlutusTx.applyCode` PlutusTx.liftCode scalar

-- | Check that the Haskell version returns the correct result.
-- checkGroth16Verify_Haskell :: Bool
-- checkGroth16Verify_Haskell =
--     groth16Verify (g1 alpha) (g2 beta) (g2 gamma) (g2 delta)
--                       (g1 gamma_abc_1) (g1 gamma_abc_2) (g1 a) (g2 b) (g1 c) scalar

verifyBlsGroth16PolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsGroth16PolicyScriptV2 = policyScript verifyBlsGroth16PolicyV2

verifyBlsGroth16AssetIdV2 :: C.AssetId
verifyBlsGroth16AssetIdV2 = C.AssetId (policyIdV2 verifyBlsGroth16PolicyV2) blsAssetName

verifyBlsGroth16Redeemer :: C.HashableScriptData
verifyBlsGroth16Redeemer = toScriptData groth16Scalar

verifyBlsGroth16MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsGroth16MintWitnessV2 era =
    (policyIdV2 verifyBlsGroth16PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsGroth16PolicyScriptV2) verifyBlsGroth16Redeemer)

---- BLS signature with the public key over G1 ----

sigG1ParamsWithPubkey :: BlsParamsWithPubKey
sigG1ParamsWithPubkey = BlsParamsWithPubKey
  { pubMessage = "3e00ef2f895f40d67f5bb8e81f09a5a12c840ec3ce9a7f3b181be188ef711a1e"
  , pubKey = "aa04a34d4db073e41505ebb84eee16c0094fde9fa22ec974adb36e5b3df5b2608639f091bff99b5f090b3608c3990173"
  , pubSig = "808ccec5435a63ae01e10d81be2707ab55cd0dfc235dfdf9f70ad32799e42510d67c9f61d98a6578a96a76cf6f4c105d09262ec1d86b06515360b290e7d52d347e48438de2ea2233f3c72a0c2221ed2da5e115367bca7a2712165032340e0b29"
  }

{-# INLINABLE mkVerifySigG1 #-}
mkVerifySigG1 :: BlsParamsWithPubKey -> sc -> Bool
mkVerifySigG1 BlsParamsWithPubKey{..} _sc = do
  let
    pkDeser = BI.bls12_381_G1_uncompress pubKey
    sigDeser = BI.bls12_381_G2_uncompress pubSig
    hashedMsg = BI.bls12_381_G2_hashToGroup pubMessage blsSigBls12381G2XmdSha256SswuRoNul

  BI.bls12_381_finalVerify (BI.bls12_381_millerLoop pkDeser hashedMsg) (BI.bls12_381_millerLoop g1 sigDeser)

verifyBlsSigG1PolicyV2 :: MintingPolicy
verifyBlsSigG1PolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifySigG1

verifyBlsSigG1PolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsSigG1PolicyScriptV2 = policyScript verifyBlsSigG1PolicyV2

verifyBlsSigG1AssetIdV2 :: C.AssetId
verifyBlsSigG1AssetIdV2 = C.AssetId (policyIdV2 verifyBlsSigG1PolicyV2) blsAssetName

verifyBlsSigG1Redeemer :: C.HashableScriptData
verifyBlsSigG1Redeemer = toScriptData sigG1ParamsWithPubkey

verifyBlsSigG1MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSigG1MintWitnessV2 era =
    (policyIdV2 verifyBlsSigG1PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSigG1PolicyScriptV2) verifyBlsSigG1Redeemer)

---- BLS signature with the public key over G2 ----

sigG2ParamsWithPubkey :: BlsParamsWithPubKey
sigG2ParamsWithPubkey = BlsParamsWithPubKey
  { pubMessage = "5032ec38bbc5da98ee0c6f568b872a65a08abf251deb21bb4b56e5d8821e68aa"
  , pubKey = "b4953c4ba10c4d4196f90169e76faf154c260ed73fc77bb65dc3be31e0cec614a7287cda94195343676c2c57494f0e651527e6504c98408e599a4eb96f7c5a8cfb85d2fdc772f28504580084ef559b9b623bc84ce30562ed320f6b7f65245ad4"
  , pubSig = "a9d4de7b0b2805fe52bccb86415ef7b8ffecb313c3c254044dfc1bdc531d3eae999d87717822a052692140774bd7245c"
  }

{-# INLINABLE mkVerifySigG2 #-}
mkVerifySigG2 :: BlsParamsWithPubKey -> sc -> Bool
mkVerifySigG2 BlsParamsWithPubKey{..} _sc = do
  let
    pkDeser = BI.bls12_381_G2_uncompress pubKey
    sigDeser = BI.bls12_381_G1_uncompress pubSig
    hashedMsg = BI.bls12_381_G1_hashToGroup pubMessage blsSigBls12381G2XmdSha256SswuRoNul

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
verifyBlsSigG2Redeemer = toScriptData sigG2ParamsWithPubkey

verifyBlsSigG2MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSigG2MintWitnessV2 era =
    (policyIdV2 verifyBlsSigG2PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSigG2PolicyScriptV2) verifyBlsSigG2Redeemer)

---- BLS aggregate signature with same key and different messages with public key over G1 ----

aggregateSigG1Params :: BlsParamsWithAggregateSignature
aggregateSigG1Params = BlsParamsWithAggregateSignature
  { aggregateSigMessages =
    [ "2ba037cdb63cb5a7277dc5d6dc549e4e28a15c70670f0e97787c170485829264"
    , "ecbf14bddeb68410f423e8849e0ce35c10d20a802bbc3d9a6ca01c386279bf01"
    , "e8f75f478cb0d159db767341602fa02d3e01c3d9aacf9b686eccf1bb5ff4c8fd"
    , "21473e89d50f51f9a1ced2390c72ee7e37f15728e61d1fb2c8c839495e489052"
    , "8c146d00fe2e1caec31b159fc42dcd7e06865c6fa5267c6ca9c5284e651e175a"
    , "362f469b6e722347de959f76533315542ffa440d37cde8862da3b3331e53b60d"
    , "73baeb620e63a2e646ea148974350aa337491e5f5fc087cb429173d1eeb74f5a"
    , "73acc6c3d72b59b8bf5ab58cdcf76aa001689aac938a75b1bb25d77b5382898c"
    , "4e73ba04bae3a083c8a2109f15b8c4680ae4ba1c70df5b513425349a77e95d3b"
    , "565825a0227d45068e61eb90aa1a4dc414c0976911a52d46b39f40c5849e5abe"
    ]
  , aggregateSigPubKey = "97c919babda8d928d771d107a69adfd85a75cee2cedc4afa4c0a7e902f38b340ea21a701a46df825210dd6942632b46c"
  , aggregateSignature = "b425291f423235b022cdd038e1a3cbdcc73b5a4470251634abb874c7585a3a05b8ea54ceb93286edb0e9184bf9a852a1138c6dd860e4b756c63dff65c433a6c5aa06834f00ac5a1a1acf6bedc44bd4354f9d36d4f20f66318f39116428fabb88"
  }

{-# INLINABLE mkAggregateSigG1 #-}
mkAggregateSigG1 :: BlsParamsWithPubKey -> sc -> Bool
mkAggregateSigG1 BlsParamsWithPubKey{..} _sc = do
  let
    hashedMsgs = mapWithConstSecondArg BI.bls12_381_G2_hashToGroup blsSigBls12381G2XmdSha256SswuRoNul messageElements
    pkDeser = BI.bls12_381_G1_uncompress aggregateSigPubKey
    aggrSigDeser = BI.bls12_381_G1_uncompress aggregateSignature
    aggrMsg = foldl1 BI.bls12_381_G1_add hashedMsgs

  BI.bls12_381_finalVerify (BI.bls12_381_millerLoop pkDeser aggrMsg) (BI.bls12_381_millerLoop g1 aggrSigDeser)

    where
      mapWithConstSecondArg :: (a -> b) -> a -> [c] -> [b]
      mapWithConstSecondArg f arg2 = map (\x -> f x arg2)

  --  * hashed_msg_i = G2HashToCurve(msg_i, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_") for i in [1, 10]
  --  * pk_deser = G1Decompress(pk)
  --  * aggr_sig_deser = G2Decompress(aggr_sig)
  --  * aggr_msg = sum_{i\in[1,10]} hashed_msg_i
  --  * Check that pairing(pk_deser, aggr_msg) = pairing(G1Generator, aggr_sig_deser)

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
verifyBlsSigG2Redeemer = toScriptData sigG2ParamsWithPubkey

verifyBlsSigG2MintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSigG2MintWitnessV2 era =
    (policyIdV2 verifyBlsSigG2PolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsSigG2PolicyScriptV2) verifyBlsSigG2Redeemer)
