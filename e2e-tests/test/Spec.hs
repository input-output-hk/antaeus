{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main(main) where

import CardanoTestnet qualified as TN
import Control.Exception (SomeException)
import Control.Exception.Base (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, readIORef)
import Data.Time.Clock.POSIX qualified as Time
import GHC.IORef (newIORef)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as HE
import Helpers.Test (TestParams (TestParams), runTest, runTestWithPosixTime)
import Helpers.TestResults (TestResult, TestSuiteResults (..), testSuitesToJUnit)
import Helpers.Testnet qualified as TN
import Helpers.Utils qualified as U
import Spec.AlonzoFeatures qualified as Alonzo
import Spec.BabbageFeatures qualified as Babbage
import Spec.Builtins.SECP256k1 qualified as Builtins
import System.Directory (createDirectoryIfMissing)
import Test.Base qualified as H
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.XML.Light (showTopElement)


main :: IO ()
main = do
  runTestsWithResults


tests :: IORef [TestResult] -> IORef [TestResult] -> IORef [TestResult] ->  TestTree
tests pv6ResultsRef pv7ResultsRef pv8ResultsRef = testGroup "Plutus E2E Tests" [
  testProperty "Alonzo PV6 Tests" (pv6Tests pv6ResultsRef)
  , testProperty "Babbage PV7 Tests" (pv7Tests pv7ResultsRef)
  , testProperty "Babbage PV8 Tests" (pv8Tests pv8ResultsRef)
--   , testProperty "debug" (debugTests pv8ResultsRef)
--   , testProperty "Babbage PV8 Tests (on Preview testnet)" (localNodeTests pv8ResultsRef TN.localNodeOptionsPreview)
  ]

pv6Tests :: IORef [TestResult] -> H.Property
pv6Tests resultsRef = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsAlonzo6
    preTestnetTime <- liftIO Time.getPOSIXTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath
        run name test = runTest name test resultsRef options testParams
        runWithPosixTime name test = runTestWithPosixTime name test resultsRef options testParams preTestnetTime

    sequence_
      [ runWithPosixTime Alonzo.checkTxInfoV1TestInfo Alonzo.checkTxInfoV1Test
      , run Alonzo.datumHashSpendTestInfo Alonzo.datumHashSpendTest
      , run Alonzo.mintBurnTestInfo Alonzo.mintBurnTest
      , run Alonzo.collateralContainsTokenErrorTestInfo Alonzo.collateralContainsTokenErrorTest
      , run Alonzo.noCollateralInputsErrorTestInfo Alonzo.noCollateralInputsErrorTest
      , run Alonzo.missingCollateralInputErrorTestInfo Alonzo.missingCollateralInputErrorTest
      , run Alonzo.tooManyCollateralInputsErrorTestInfo Alonzo.tooManyCollateralInputsErrorTest
      , run Builtins.verifySchnorrAndEcdsaTestInfo Builtins.verifySchnorrAndEcdsaTest
      ]

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes


pv7Tests :: IORef [TestResult] ->  H.Property
pv7Tests resultsRef = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsBabbage7
    preTestnetTime <- liftIO Time.getPOSIXTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath
        run name test = runTest name test resultsRef options testParams
        runWithPosixTime name test = runTestWithPosixTime name test resultsRef options testParams preTestnetTime

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    sequence_
      [  runWithPosixTime Alonzo.checkTxInfoV1TestInfo Alonzo.checkTxInfoV1Test
       , runWithPosixTime Babbage.checkTxInfoV2TestInfo Babbage.checkTxInfoV2Test
       , run Alonzo.datumHashSpendTestInfo Alonzo.datumHashSpendTest
       , run Alonzo.mintBurnTestInfo Alonzo.mintBurnTest
       , run Alonzo.collateralContainsTokenErrorTestInfo Alonzo.collateralContainsTokenErrorTest
       , run Alonzo.noCollateralInputsErrorTestInfo Alonzo.noCollateralInputsErrorTest
       , run Alonzo.missingCollateralInputErrorTestInfo Alonzo.missingCollateralInputErrorTest
       , run Alonzo.tooManyCollateralInputsErrorTestInfo Alonzo.tooManyCollateralInputsErrorTest
       , run Builtins.verifySchnorrAndEcdsaTestInfo Builtins.verifySchnorrAndEcdsaTest
       , run Babbage.referenceScriptMintTestInfo Babbage.referenceScriptMintTest
       , run Babbage.referenceScriptInlineDatumSpendTestInfo Babbage.referenceScriptInlineDatumSpendTest
       , run Babbage.referenceScriptDatumHashSpendTestInfo Babbage.referenceScriptDatumHashSpendTest
       , run Babbage.inlineDatumSpendTestInfo Babbage.inlineDatumSpendTest
       , run Babbage.referenceInputWithV1ScriptErrorTestInfo Babbage.referenceInputWithV1ScriptErrorTest
       , run Babbage.referenceScriptOutputWithV1ScriptErrorTestInfo Babbage.referenceScriptOutputWithV1ScriptErrorTest
       , run Babbage.inlineDatumOutputWithV1ScriptErrorTestInfo Babbage.inlineDatumOutputWithV1ScriptErrorTest
      ]

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

pv8Tests :: IORef [TestResult] -> H.Property
pv8Tests resultsRef = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsBabbage8
    preTestnetTime <- liftIO Time.getPOSIXTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath
        run name test = runTest name test resultsRef options testParams
        runWithPosixTime name test = runTestWithPosixTime name test resultsRef options testParams preTestnetTime

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    sequence_
      [  runWithPosixTime Alonzo.checkTxInfoV1TestInfo Alonzo.checkTxInfoV1Test
       , runWithPosixTime Babbage.checkTxInfoV2TestInfo Babbage.checkTxInfoV2Test
       , run Alonzo.datumHashSpendTestInfo Alonzo.datumHashSpendTest
       , run Alonzo.mintBurnTestInfo Alonzo.mintBurnTest
       , run Alonzo.collateralContainsTokenErrorTestInfo Alonzo.collateralContainsTokenErrorTest
       , run Alonzo.noCollateralInputsErrorTestInfo Alonzo.noCollateralInputsErrorTest
       , run Alonzo.missingCollateralInputErrorTestInfo Alonzo.missingCollateralInputErrorTest
       , run Alonzo.tooManyCollateralInputsErrorTestInfo Alonzo.tooManyCollateralInputsErrorTest
       , run Builtins.verifySchnorrAndEcdsaTestInfo Builtins.verifySchnorrAndEcdsaTest
       , run Babbage.referenceScriptMintTestInfo Babbage.referenceScriptMintTest
       , run Babbage.referenceScriptInlineDatumSpendTestInfo Babbage.referenceScriptInlineDatumSpendTest
       , run Babbage.referenceScriptDatumHashSpendTestInfo Babbage.referenceScriptDatumHashSpendTest
       , run Babbage.inlineDatumSpendTestInfo Babbage.inlineDatumSpendTest
       , run Babbage.referenceInputWithV1ScriptErrorTestInfo Babbage.referenceInputWithV1ScriptErrorTest
       , run Babbage.referenceScriptOutputWithV1ScriptErrorTestInfo Babbage.referenceScriptOutputWithV1ScriptErrorTest
       , run Babbage.inlineDatumOutputWithV1ScriptErrorTestInfo Babbage.inlineDatumOutputWithV1ScriptErrorTest
       , run Babbage.returnCollateralWithTokensValidScriptTestInfo Babbage.returnCollateralWithTokensValidScriptTest
       , run Babbage.submitWithInvalidScriptThenCollateralIsTakenAndReturnedTestInfo Babbage.submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
      ]

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

debugTests :: IORef [TestResult] -> H.Property
debugTests resultsRef = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    let options = TN.testnetOptionsBabbage8
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    runTest Alonzo.noCollateralInputsErrorTestInfo Alonzo.noCollateralInputsErrorTest resultsRef options testParams

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

localNodeTests :: IORef [TestResult] -> Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
localNodeTests resultsRef options = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do
    --preTestnetTime <- liftIO Time.getPOSIXTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath
        run name test = runTest name test resultsRef options testParams

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    -- TODO: pass in or query for slot range to use in checkTxInfo tests
    --runTestWithPosixTime "checkTxInfoV1Test" Alonzo.checkTxInfoV1Test options testParams preTestnetTime
    --runTestWithPosixTime "checkTxInfoV2Test" Babbage.checkTxInfoV2Test options testParams preTestnetTime
    run Builtins.verifySchnorrAndEcdsaTestInfo Builtins.verifySchnorrAndEcdsaTest
    run Babbage.referenceScriptMintTestInfo Babbage.referenceScriptMintTest
    run Babbage.referenceScriptInlineDatumSpendTestInfo Babbage.referenceScriptInlineDatumSpendTest
    run Babbage.referenceScriptDatumHashSpendTestInfo Babbage.referenceScriptDatumHashSpendTest

    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

runTestsWithResults :: IO ()
runTestsWithResults = do
  createDirectoryIfMissing False "test-report-xml"

  [pv6ResultsRef, pv7ResultsRef, pv8ResultsRef] <- traverse newIORef [[], [], []]

  -- Catch the exception returned by defaultMain to proceed with report generation
  _ <- try (defaultMain $ tests pv6ResultsRef pv7ResultsRef pv8ResultsRef) :: IO (Either SomeException ())

  [pv6Results, pv7Results, pv8Results] <- traverse readIORef [pv6ResultsRef, pv7ResultsRef, pv8ResultsRef]
  -- putStrLn $ "Debug final results: " ++ show results -- REMOVE

  let
    pv6TestSuiteResult = TestSuiteResults "Alonzo PV6 Tests"  pv6Results
    pv7TestSuiteResult = TestSuiteResults "Babbage PV7 Tests" pv7Results
    pv8TestSuiteResult = TestSuiteResults "Babbage PV8 Tests" pv8Results

  -- Use 'results' to generate custom JUnit XML report
  let xml = testSuitesToJUnit [pv6TestSuiteResult, pv7TestSuiteResult, pv8TestSuiteResult]
  -- putStrLn $ "Debug XML: " ++ showTopElement xml -- REMOVE
  writeFile "test-report-xml/test-results.xml" $ showTopElement xml
