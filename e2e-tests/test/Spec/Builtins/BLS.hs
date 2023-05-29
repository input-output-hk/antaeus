{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Spec.Builtins.BLS where

import Cardano.Api qualified as C
import Data.Map qualified as Map

import Control.Monad.IO.Class (MonadIO)
import Hedgehog (MonadTest)
import Hedgehog.Internal.Property (annotate)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import PlutusScripts.BLS qualified as PS

simpleVerifyBls12381SigTestInfo = TestInfo {
    testName = "VerifyBls12381SigTest",
    testDescription = "Uses three scripts with BLS functions." ++
                      "The first is a simple script that calculats a public key, hash a message, create signature and verify the message with signature." ++
                      "The second is a verifiable random function (VRF) example" ++
                      "The third is verification of a zk Groth16 proof",
    test = simpleVerifyBls12381SigTest}
bls12381SigTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
bls12381SigTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  pv <- TN.pvFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

-- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.verifyBlsSimpleAssetIdV2, 2), (PS.verifyBlsVrfAssetIdV2, 4), (PS.verifyBlsGroth16AssetIdV2, 8)]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
    mintWitnesses = Map.fromList [PS.verifyBlsSimpleMintWitnessV2, PS.verifyBlsVrfMintWitnessV2, PS.verifyBlsGroth16MintWitnessV2]
    collateral = Tx.txInsCollateral era [txIn]
    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  -- Build and submit transaction
  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0

  -- Query for txo and assert it contains newly minting tokens to prove successful use of BLS builtins
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "TN.getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue


