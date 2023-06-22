{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Spec.AlonzoFeatures where

import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Helpers.Common (makeAddress)
import Helpers.Query qualified as Q
import Helpers.Test (assert, success)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import PlutusLedgerApi.V1 as PlutusV1 hiding (lowerBound, upperBound)
import PlutusLedgerApi.V1.Interval as P hiding (lowerBound, upperBound)
import PlutusLedgerApi.V1.Time as P
import PlutusScripts.Always qualified as PS
import PlutusScripts.Helpers qualified as PS
import PlutusScripts.V1TxInfo qualified as PS (checkV1TxInfoAssetIdV1, checkV1TxInfoMintWitnessV1,
                                               checkV1TxInfoRedeemer, txInfoData, txInfoFee, txInfoInputs, txInfoMint,
                                               txInfoOutputs, txInfoSigs)


checkTxInfoV1TestInfo = TestInfo {
    testName = "checkTxInfoV1Test",
    testDescription = "Check each attribute of the TxInfo from the V1 ScriptContext in a single transaction",
    test = checkTxInfoV1Test
    }
checkTxInfoV1Test :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
checkTxInfoV1Test networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath, mTime} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, w1VKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"

  let
    tokenValues = C.valueFromList [(PS.checkV1TxInfoAssetIdV1, 1)]
    executionUnits = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 10_000_000 }
    collateral = Tx.txInsCollateral era [txIn]
    totalLovelace = C.txOutValueToLovelace txInValue
    fee = 2_000_000 :: C.Lovelace
    amountPaid = 10_000_000
    amountReturned = totalLovelace - amountPaid - fee
    datum = PS.toScriptData (42 ::Integer)

    txOut1 = Tx.txOutWithDatumInTx era (C.lovelaceToValue amountPaid <> tokenValues) w1Address datum
    txOut2 = Tx.txOut era (C.lovelaceToValue amountReturned) w1Address

    lowerBound = P.fromMilliSeconds
      $ P.DiffMilliSeconds $ U.posixToMilliseconds $ fromJust mTime -- before slot 1
    upperBound = P.fromMilliSeconds
      -- ~10mins after slot 1 (to account for testnet init time)
      $ P.DiffMilliSeconds $ U.posixToMilliseconds startTime + 600_000
    timeRange = P.interval lowerBound upperBound :: PlutusV1.POSIXTimeRange

    expTxInfoInputs     = PS.txInfoInputs (txIn, txInAsTxOut)
    expTxInfoOutputs    = PS.txInfoOutputs [ txOut1, txOut2 ]
    expTxInfoFee        = PS.txInfoFee fee
    expTxInfoMint       = PS.txInfoMint tokenValues
    expDCert            = [] -- not testing any staking registration certificate
    expWdrl             = [] -- not testing any staking reward withdrawal
    expTxInfoSigs       = PS.txInfoSigs [w1VKey]
    expTxInfoData       = PS.txInfoData [datum]
    expTxInfoValidRange = timeRange

    redeemer = PS.checkV1TxInfoRedeemer [expTxInfoInputs] expTxInfoOutputs expTxInfoFee expTxInfoMint
               expDCert expWdrl expTxInfoValidRange expTxInfoSigs expTxInfoData
    mintWitnesses = Map.fromList [PS.checkV1TxInfoMintWitnessV1 era redeemer executionUnits]

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut1, txOut2]
      , C.txFee = Tx.txFee era fee
      , C.txValidityRange = Tx.txValidityRange era 1 2700
      -- ^ ~9min range (200ms slots)
      -- ^ Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch)
      , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
      }
  txbody <- Tx.buildRawTx era txBodyContent
  kw <- Tx.signTx era txbody w1SKey
  let signedTx = C.makeSignedTransaction [kw] txbody

  Tx.submitTx era localNodeConnectInfo signedTx

  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue

datumHashSpendTestInfo = TestInfo {
    testName = "datumHashSpendTest",
    testDescription = "Test spending outputs with datum hash both with and without datum value embedded in tx body",
    test = datumHashSpendTest }
datumHashSpendTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
datumHashSpendTest networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  let nodeSocket = C.localNodeSocketPath localNodeConnectInfo
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction with two script outputs to be spent
  -- only one has its datum value embedded in the tx body

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV1) networkId
    datum1         = PS.toScriptData (1 :: Integer)
    datum2         = PS.toScriptData (2 :: Integer)
    scriptTxOut1   = Tx.txOutWithDatumHash era (C.lovelaceToValue 5_000_000) scriptAddress datum1
    scriptTxOut2   = Tx.txOutWithDatumInTx era (C.lovelaceToValue 5_000_000) scriptAddress datum2
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [scriptTxOut1, scriptTxOut2, otherTxOut]
      }

  signedTx <- Tx.buildTx era nodeSocket txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let txInAtScript1  = Tx.txIn (Tx.txId signedTx) 0
      txInAtScript2  = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn      = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript1 "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    scriptTxIn1 = Tx.txInWitness txInAtScript1 $ PS.alwaysSucceedSpendWitnessV1 era Nothing (Just datum1)
    scriptTxIn2 = Tx.txInWitness txInAtScript2 $ PS.alwaysSucceedSpendWitnessV1 era Nothing (Just datum2)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn1, scriptTxIn2]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era nodeSocket txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let
    expectedTxIn1 = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains expected ada value
  resultTxOut1 <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn1 "resultTxOut1 <- getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut1 adaValue
  H.assert txOutHasAdaValue

  success

mintBurnTestInfo = TestInfo {
    testName = "mintBurnTest",
    testDescription = "Mint some tokens with Plutus policy in one transaction and then burn some of " ++
                      "them in second transaction",
    test = mintBurnTest}
mintBurnTest :: (MonadTest m, MonadIO m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
mintBurnTest networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  let nodeSocket = C.localNodeSocketPath localNodeConnectInfo
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction to mint tokens

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 10)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOut = Tx.txOut era (C.lovelaceToValue 10_000_000 <> tokenValues) w1Address
    otherTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut, otherTxOut]
      }

  signedTx <- Tx.buildTx era nodeSocket txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn    = Tx.txIn (Tx.txId signedTx) 1
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  -- build a transaction to burn tokens

  let
    txIn2 = expectedTxIn
    burnValue = C.valueFromList [(PS.alwaysSucceedAssetIdV1, -5)]
    tokenValues2 = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 5)]
    collateral2 = Tx.txInsCollateral era [otherTxIn]
    txOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000 <> tokenValues2) w1Address
    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn2]
      , C.txInsCollateral = collateral2
      , C.txMintValue = Tx.txMintValue era burnValue mintWitnesses
      , C.txOuts = [txOut2]
      }
  signedTx2 <- Tx.buildTx era nodeSocket txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn2 = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains tokens remaining after burn
  resultTxOut2 <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn2 "resultTxOut2 <- getTxOutAtAddress"
  txOutHasTokenValue2 <- Q.txOutHasValue resultTxOut2 tokenValues2
  assert "txOut has tokens" txOutHasTokenValue2

collateralContainsTokenErrorTestInfo = TestInfo {
    testName = "collateralContainsTokenErrorTest",
    testDescription = "CollateralContainsNonADA error occurs when including tokens in a collateral input",
    test = collateralContainsTokenErrorTest}
collateralContainsTokenErrorTest :: (MonadTest m, MonadIO m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  --Maybe POSIXTime ->
  m (Maybe String)
collateralContainsTokenErrorTest networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  let nodeSocket = C.localNodeSocketPath localNodeConnectInfo
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction to mint tokens

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 1)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOut = Tx.txOut era (C.lovelaceToValue 10_000_000 <> tokenValues) w1Address
    otherTxOut = Tx.txOut era (C.lovelaceToValue 8_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut, otherTxOut]
      }

  signedTx <- Tx.buildTx era nodeSocket txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn = Tx.txIn (Tx.txId signedTx) 1
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  -- build a transaction to mint again but using a collateral input that contains a native token

  let
    txInWithToken = expectedTxIn
    collateral2 = Tx.txInsCollateral era [txInWithToken]
    txOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000 <> tokenValues) w1Address
    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [otherTxIn]
      , C.txInsCollateral = collateral2
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut2]
      }

  signedTx2 <- Tx.buildTx era nodeSocket txBodyContent2 w1Address w1SKey networkId

  eitherSubmit <- Tx.submitTx' era localNodeConnectInfo signedTx2
  -- Not sure why this ledger error is doesn't occur when balancing (it does with cardano-cli)
  -- asserting for it on submit instead
  let expError = "CollateralContainsNonADA"
  assert expError $ Tx.isSubmitError expError eitherSubmit

missingCollateralInputErrorTestInfo = TestInfo {
    testName = "missingCollateralInputErrorTest",
    testDescription = "TxBodyEmptyTxInsCollateral error occurs when collateral input is required but txbody's " ++
                      "txInsCollateral is missing",
    test = missingCollateralInputErrorTest}
missingCollateralInputErrorTest :: (MonadTest m, MonadIO m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
missingCollateralInputErrorTest networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  let nodeSocket = C.localNodeSocketPath localNodeConnectInfo
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction to mint tokens

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 1)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    txOut = Tx.txOut era (C.lovelaceToValue 10_000_000 <> tokenValues) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  eitherTx <- Tx.buildTx' era nodeSocket txBodyContent w1Address w1SKey networkId
  let expError = "TxBodyEmptyTxInsCollateral"
  assert expError $ Tx.isTxBodyError expError eitherTx

noCollateralInputsErrorTestInfo = TestInfo {
    testName = "noCollateralInputsErrorTest",
    testDescription = "NoCollateralInputs error occurs when collateral is required but txbody's " ++
                      "txInsCollateral is empty",
    test = noCollateralInputsErrorTest}
noCollateralInputsErrorTest :: (MonadTest m, MonadIO m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
noCollateralInputsErrorTest networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  let nodeSocket = C.localNodeSocketPath localNodeConnectInfo
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction to mint tokens

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    collateral = Tx.txInsCollateral era []
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 1)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    txOut = Tx.txOut era (C.lovelaceToValue 10_000_000 <> tokenValues) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txInsCollateral = collateral -- txInsCollateral exists but with empty list
      , C.txOuts = [txOut]
      }

  signedTx <- Tx.buildTx era nodeSocket txBodyContent w1Address w1SKey networkId
  eitherSubmit <- Tx.submitTx' era localNodeConnectInfo signedTx
  -- this ledger error isn't caught by balancing so asserting for it on submit instead
  let expError = "NoCollateralInputs"
  assert expError $ Tx.isSubmitError expError eitherSubmit

tooManyCollateralInputsErrorTestInfo = TestInfo {
    testName = "tooManyCollateralInputsErrorTest",
    testDescription = "TooManyCollateralInputs error occurs when number of collateral inputs exceed " ++
                      "protocol param 'maxCollateralInputs'",
    test = tooManyCollateralInputsErrorTest}
tooManyCollateralInputsErrorTest :: (MonadTest m, MonadIO m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m (Maybe String)
tooManyCollateralInputsErrorTest networkOptions TestParams {localNodeConnectInfo, pparams, networkId, tempAbsPath} = do

  let nodeSocket = C.localNodeSocketPath localNodeConnectInfo
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction to mint tokens

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    maxCollateralInputs = fromIntegral $ U.unsafeFromMaybe $ C.protocolParamMaxCollateralInputs pparams
    txOut = Tx.txOut era (C.lovelaceToValue 1_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = replicate (maxCollateralInputs + 1) txOut -- one more than max
      }

  signedTx <- Tx.buildTx era nodeSocket txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let collateralTxIns = map (\i -> Tx.txIn (Tx.txId signedTx) i) [0 .. maxCollateralInputs]
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address (head collateralTxIns) "waitForTxInAtAddress"

  -- build a transaction to mint again but using a collateral input that contains a native token

  let
    collateral = Tx.txInsCollateral era collateralTxIns
    txOut2 = Tx.txOut era (C.lovelaceToValue 1_000_000) w1Address
    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns (take 3 collateralTxIns)
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut2]
      }

  signedTx2 <- Tx.buildTx era nodeSocket txBodyContent2 w1Address w1SKey networkId

  eitherSubmit <- Tx.submitTx' era localNodeConnectInfo signedTx2
  -- this ledger error isn't caught by balancing so asserting for it on submit instead
  let expError = "TooManyCollateralInputs"
  assert expError $ Tx.isSubmitError expError eitherSubmit

-- TODO: tx to produce error: InsufficientCollateral
-- TODO: collateral input at script address error
