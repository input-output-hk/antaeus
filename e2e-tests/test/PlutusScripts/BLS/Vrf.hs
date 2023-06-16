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

module PlutusScripts.BLS.SimpleSignVerify (
    verifyBlsVrfAssetIdV2
  , verifyBlsVrfMintWitnessV2
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
    privKey = 50166937291276222007610100461546392414157570314060957244808461481762532157524
  , message  = "I am a message"
  }

---- Verify BLS VRF ----

{- | A basic VRF using G2 (using G1 would be cheaper)
  for reference see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-15#name-elliptic-curve-vrf-ecvrf
  and more readable and mathematical in nature see https://eprint.iacr.org/2017/099.pdf.
-}

{-# INLINABLE mkVerifyBlsVrfPolicy #-}
mkVerifyBlsVrfPolicy ::
     BuiltinBLS12_381_G2_Element
  -> BlsParams
  -> sc
  -> Bool
mkVerifyBlsVrfPolicy g2Gen BlsParams{..} _sc = do
  let
    -- calculate public key
    pub = BI.bls12_381_G2_scalarMul privKey g2

    -- hash this msg to G2
    h = BI.bls12_381_G2_hashToGroup message BI.emptyByteString

    -- define first element of the proof of correct VRF
    gamma = BI.bls12_381_G2_scalarMul privKey h

    -- for this signed hash with preimage alpha, define a ephemeral interger (for each signature take a new one). Random 32 byte int
    k = 108204667002115086588596846168569722098834602153875763359385781912495445631691 :: Integer

    -- define second element of the proof of correct VRF
    -- the paper notes that this can actually be truncated to 128 bits without loss of the 128 bits security.
    -- truncating this will allow for smaller proof sizes.
    c = sha2_256 . mconcat $ BI.bls12_381_G2_compress <$> [g2Gen, h, pub, gamma, BI.bls12_381_G2_scalarMul k g2Gen, BI.bls12_381_G2_scalarMul k h]

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
    u  = BI.bls12_381_G2_add (BI.bls12_381_G2_scalarMul (os2ip c) pub) (BI.bls12_381_G2_scalarMul s g2Gen)
    h' = BI.bls12_381_G2_hashToGroup message BI.emptyByteString
    v  = BI.bls12_381_G2_add (BI.bls12_381_G2_scalarMul (os2ip c) gamma) (BI.bls12_381_G2_scalarMul s h')

  -- and check
  c == (sha2_256 . mconcat $ BI.bls12_381_G2_compress <$> [g2Gen,h',pub,gamma,u,v])

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
verifyBlsVrfPolicyV2 = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode g2Generator
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifyBlsVrfPolicy

verifyBlsVrfPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyBlsVrfPolicyScriptV2 = policyScript verifyBlsVrfPolicyV2

verifyBlsVrfAssetIdV2 :: C.AssetId
verifyBlsVrfAssetIdV2 = C.AssetId (policyIdV2 verifyBlsVrfPolicyV2) blsAssetName

verifyBlsVrfRedeemer :: C.HashableScriptData
verifyBlsVrfRedeemer = toScriptData redeemerParams

verifyBlsVrfMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsVrfMintWitnessV2 era =
    (policyIdV2 verifyBlsVrfPolicyV2,
     mintScriptWitness era plutusL2 (Left verifyBlsVrfPolicyScriptV2) verifyBlsVrfRedeemer)
