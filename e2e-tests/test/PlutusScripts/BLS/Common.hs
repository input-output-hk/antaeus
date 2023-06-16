{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use underscore" #-}

module PlutusScripts.BLS.Common where

import Cardano.Api qualified as C
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

blsAssetName :: C.AssetName
blsAssetName = C.AssetName "BLS"

-- hex value 424c535f5349475f424c53313233383147325f584d443a5348412d3235365f535357555f524f5f4e554c5f"
blsSigBls12381G2XmdSha256SswuRoNul :: P.BuiltinByteString
blsSigBls12381G2XmdSha256SswuRoNul = BI.toBuiltin $ C8.pack "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"

-- all zero bytestring 16 byte in length used to append to truncated bytestring onchain
{-# INLINABLE byteString16Null #-}
byteString16Null :: P.BuiltinByteString
byteString16Null = toBuiltin $ bytesFromHex "00000000000000000000000000000000"

-- G1 and G2 generators
{-# INLINABLE g1Generator #-}
g1Generator = BI.bls12_381_G1_uncompress $ toBuiltin $ BS.pack
  [151, 241, 211, 167, 49, 151, 215, 148, 38, 149, 99, 140, 79, 169, 172, 15, 195, 104, 140, 79, 151, 116, 185,
   5, 161,78, 58, 63, 23, 27, 172, 88, 108, 85, 232, 63, 249, 122, 26, 239, 251, 58, 240, 10, 219, 34, 198, 187]
{-# INLINABLE g2Generator #-}
g2Generator = BI.bls12_381_G2_uncompress $ toBuiltin $ BS.pack
  [147, 224, 43, 96, 82, 113, 159, 96, 125, 172, 211, 160, 136, 39, 79, 101, 89, 107, 208, 208, 153, 32, 182,
   26, 181, 218, 97, 187, 220, 127, 80, 73, 51, 76, 241, 18, 19, 148, 93, 87, 229, 172, 125, 5, 93, 4, 43, 126,
   2, 74, 162, 178, 240, 143, 10, 145, 38, 8, 5, 39, 45, 197, 16, 81, 198, 228, 122, 212, 250, 64, 59, 2, 180,
   81, 11, 100, 122, 227, 209, 119, 11, 172, 3, 38, 168, 5, 187, 239, 212, 128, 86, 200, 193, 33, 189, 184]
