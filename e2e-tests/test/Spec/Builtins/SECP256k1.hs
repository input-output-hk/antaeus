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
{-# LANGUAGE NamedFieldPuns      #-}

module Spec.Builtins.SECP256k1 where

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
import PlutusScripts.SECP256k1 qualified as PS

verifySchnorrAndEcdsaTestInfo = TestInfo {
    testName = "verifySchnorrAndEcdsaTest",
    testDescription = "SECP256k1 builtin verify functions verifySchnorrSecp256k1Signature and " ++
                      "verifyEcdsaSecp256k1Signature can only be used to mint in Babbage era protocol version 8 " ++
                      "and beyond.",
    test = verifySchnorrAndEcdsaTest}
verifySchnorrAndEcdsaTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
verifySchnorrAndEcdsaTest networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  let nodeSocket = C.localNodeSocketPath localNodeConnectInfo
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  pv <- TN.pvFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

-- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    (verifySchnorrAssetId, verifyEcdsaAssetId, verifySchnorrMintWitness, verifyEcdsaMintWitness, plutusVersion) =
      case era of
        C.AlonzoEra  ->
          ( PS.verifySchnorrAssetIdV1,
            PS.verifyEcdsaAssetIdV1,
            PS.verifySchnorrMintWitnessV1 era,
            PS.verifyEcdsaMintWitnessV1 era,
            "PlutusV1" )
        C.BabbageEra ->
          ( PS.verifySchnorrAssetIdV2,
            PS.verifyEcdsaAssetIdV2,
            PS.verifySchnorrMintWitnessV2 era,
            PS.verifyEcdsaMintWitnessV2 era,
            "PlutusV2" )

    tokenValues = C.valueFromList [(verifySchnorrAssetId, 4), (verifyEcdsaAssetId, 2)]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
    mintWitnesses = Map.fromList [verifySchnorrMintWitness, verifyEcdsaMintWitness]
    collateral = Tx.txInsCollateral era [txIn]
    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  case pv < 8 of
    True -> do
      -- Assert that "forbidden" error occurs when attempting to use either SECP256k1 builtin
      eitherTx <- Tx.buildTx' era nodeSocket txBodyContent w1Address w1SKey networkId
      annotate $ show eitherTx
      let
        expErrorSchnorr = "Builtin function VerifySchnorrSecp256k1Signature is not available in language " ++
                          plutusVersion ++ " at and protocol version " ++ show pv
        expErrorEcdsa = "Builtin function VerifyEcdsaSecp256k1Signature is not available in language " ++
                        plutusVersion ++ " at and protocol version " ++ show pv
      a1 <- assert expErrorSchnorr $ Tx.isTxBodyScriptExecutionError expErrorSchnorr eitherTx
      a2 <- assert expErrorEcdsa $ Tx.isTxBodyScriptExecutionError expErrorEcdsa eitherTx
      U.concatMaybes [a1, a2]

    False -> do
      -- Build and submit transaction
      signedTx <- Tx.buildTx era nodeSocket txBodyContent w1Address w1SKey networkId
      Tx.submitTx era localNodeConnectInfo signedTx
      let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0

      -- Query for txo and assert it contains newly minting tokens to prove successful use of SECP256k1 builtins
      resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "TN.getTxOutAtAddress"
      txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
      assert "txOut has tokens" txOutHasTokenValue


