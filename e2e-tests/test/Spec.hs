{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Cardano.Api qualified as C
import Control.Exception.Base (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, readIORef)
import Data.Time.Clock.POSIX qualified as Time
import GHC.IORef (newIORef)
import Hedgehog qualified as H
import Helpers.Committee (generateCommitteeKeysAndCertificate)
import Helpers.Common (toConwayEraOnwards)
import Helpers.DRep (generateDRepKeyCredentialsAndCertificate)
import Helpers.Staking (generateStakeKeyCredentialAndCertificate)
import Helpers.Test (integrationRetryWorkspace, runTest)
import Helpers.TestData (TestParams (..))
import Helpers.TestResults (
  TestResult (..),
  TestSuiteResults (..),
  allFailureMessages,
  suiteFailureMessages,
  testSuitesToJUnit,
 )
import Helpers.Testnet qualified as TN
import Helpers.Utils qualified as U
import Spec.AlonzoFeatures qualified as Alonzo
import Spec.BabbageFeatures qualified as Babbage
import Spec.Builtins as Builtins
import Spec.ConwayFeatures qualified as Conway
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.XML.Light (showTopElement)

main :: IO ()
main = do
  runTestsWithResults

data ResultsRefs = ResultsRefs
  { pv6ResultsRef :: IORef [TestResult]
  , pv7ResultsRef :: IORef [TestResult]
  , pv8ResultsRef :: IORef [TestResult]
  , pv9ResultsRef :: IORef [TestResult]
  , pv9GovResultsRef :: IORef [TestResult]
  }

tests :: ResultsRefs -> TestTree
tests ResultsRefs{..} =
  testGroup
    "Plutus E2E Tests"
    [ -- Alonzo PV6 environment has "Chain not extended" error on start
      -- testProperty "Alonzo PV6 Tests" (pv6Tests pv6ResultsRef)
      testProperty "Babbage PV7 Tests" (pv7Tests pv7ResultsRef)
    , testProperty "Babbage PV8 Tests" (pv8Tests pv8ResultsRef)
    , testProperty "Conway PV9 Tests" (pv9Tests pv9ResultsRef)
    , testProperty "Conway PV9 Governance Tests" (pv9GovernanceTests pv9GovResultsRef)
    --  testProperty "debug" (debugTests pv8ResultsRef)
    --  testProperty
    --    "Babbage PV8 Tests (on Preview testnet)" (localNodeTests pv8ResultsRef TN.localNodeOptionsPreview)
    ]

pv6Tests :: IORef [TestResult] -> H.Property
pv6Tests resultsRef = integrationRetryWorkspace 0 "pv6" $ \tempAbsPath -> do
  let options = TN.testnetOptionsAlonzo6
  preTestnetTime <- liftIO Time.getPOSIXTime
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
    TN.setupTestEnvironment options tempAbsPath
  let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
      run testInfo = runTest testInfo resultsRef options testParams

  sequence_
    [ run Alonzo.checkTxInfoV1TestInfo
    , run Alonzo.datumHashSpendTestInfo
    , run Alonzo.mintBurnTestInfo
    , run Alonzo.collateralContainsTokenErrorTestInfo
    , run Alonzo.noCollateralInputsErrorTestInfo
    , run Alonzo.missingCollateralInputErrorTestInfo
    , -- , run Alonzo.tooManyCollateralInputsErrorTestInfo
      -- \^ fails, see https://github.com/input-output-hk/cardano-node/issues/5228
      run Builtins.verifySchnorrAndEcdsaTestInfo
    , run Builtins.verifyHashingFunctionsTestInfo
    ]

  failureMessages <- liftIO $ suiteFailureMessages resultsRef
  liftIO $ putStrLn $ "Number of test failures in suite: " ++ (show $ length failureMessages)
  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

pv7Tests :: IORef [TestResult] -> H.Property
pv7Tests resultsRef = integrationRetryWorkspace 0 "pv7" $ \tempAbsPath -> do
  let options = TN.testnetOptionsBabbage7
  preTestnetTime <- liftIO Time.getPOSIXTime
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
    TN.setupTestEnvironment options tempAbsPath
  let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
      run testInfo = runTest testInfo resultsRef options testParams

  -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
  sequence_
    [ run Alonzo.checkTxInfoV1TestInfo
    , run Babbage.checkTxInfoV2TestInfo
    , -- , run Alonzo.datumHashSpendTestInfo
      run Alonzo.mintBurnTestInfo
    , run Alonzo.collateralContainsTokenErrorTestInfo
    , run Alonzo.noCollateralInputsErrorTestInfo
    , run Alonzo.missingCollateralInputErrorTestInfo
    , run Alonzo.tooManyCollateralInputsErrorTestInfo
    , run Builtins.verifySchnorrAndEcdsaTestInfo
    , run Builtins.verifyHashingFunctionsTestInfo
    , run Babbage.referenceScriptMintTestInfo
    , run Babbage.referenceScriptInlineDatumSpendTestInfo
    , run Babbage.referenceScriptDatumHashSpendTestInfo
    , run Babbage.inlineDatumSpendTestInfo
    , run Babbage.referenceInputWithV1ScriptErrorTestInfo
    , run Babbage.referenceScriptOutputWithV1ScriptErrorTestInfo
    , run Babbage.inlineDatumOutputWithV1ScriptErrorTestInfo
    ]

  failureMessages <- liftIO $ suiteFailureMessages resultsRef
  liftIO $ putStrLn $ "Number of test failures in suite: " ++ (show $ length failureMessages)
  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

pv8Tests :: IORef [TestResult] -> H.Property
pv8Tests resultsRef = integrationRetryWorkspace 0 "pv8" $ \tempAbsPath -> do
  let options = TN.testnetOptionsBabbage8
  preTestnetTime <- liftIO Time.getPOSIXTime
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
    TN.setupTestEnvironment options tempAbsPath
  let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
      run testInfo = runTest testInfo resultsRef options testParams

  -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
  sequence_
    [ run Alonzo.checkTxInfoV1TestInfo
    , run Babbage.checkTxInfoV2TestInfo
    , -- , run Alonzo.datumHashSpendTestInfo
      run Alonzo.mintBurnTestInfo
    , run Alonzo.collateralContainsTokenErrorTestInfo
    , run Alonzo.noCollateralInputsErrorTestInfo
    , run Alonzo.missingCollateralInputErrorTestInfo
    , run Alonzo.tooManyCollateralInputsErrorTestInfo
    , run Builtins.verifySchnorrAndEcdsaTestInfo
    , run Builtins.verifyHashingFunctionsTestInfo
    , run Babbage.referenceScriptMintTestInfo
    , run Babbage.referenceScriptInlineDatumSpendTestInfo
    , run Babbage.referenceScriptDatumHashSpendTestInfo
    , run Babbage.inlineDatumSpendTestInfo
    , run Babbage.referenceInputWithV1ScriptErrorTestInfo
    , run Babbage.referenceScriptOutputWithV1ScriptErrorTestInfo
    , run Babbage.inlineDatumOutputWithV1ScriptErrorTestInfo
    , run Babbage.returnCollateralWithTokensValidScriptTestInfo
    , run Babbage.submitWithInvalidScriptThenCollateralIsTakenAndReturnedTestInfo
    ]

  failureMessages <- liftIO $ suiteFailureMessages resultsRef
  liftIO $ putStrLn $ "Number of test failures in suite: " ++ (show $ length failureMessages)
  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

pv9Tests :: IORef [TestResult] -> H.Property
pv9Tests resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
  let options = TN.testnetOptionsConway9
  preTestnetTime <- liftIO Time.getPOSIXTime
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
    TN.setupTestEnvironment options tempAbsPath
  let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
      run testInfo = runTest testInfo resultsRef options testParams

  -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
  sequence_
    [ -- NO SUPPORT FOR PlutusScriptV1 in Conway https://github.com/input-output-hk/cardano-api/issues/74
      -- run Alonzo.checkTxInfoV1TestInfo
      run Babbage.checkTxInfoV2TestInfo
    , run Conway.checkTxInfoV3TestInfo -- -- NOTE: Does not yet check V3 TxInfo fields
    , run Alonzo.datumHashSpendTestInfo
    , run Alonzo.mintBurnTestInfo
    , run Alonzo.collateralContainsTokenErrorTestInfo
    , run Alonzo.noCollateralInputsErrorTestInfo
    , run Alonzo.missingCollateralInputErrorTestInfo
    , run Alonzo.tooManyCollateralInputsErrorTestInfo
    , run Builtins.verifySchnorrAndEcdsaTestInfo
    , run Builtins.verifyHashingFunctionsTestInfo
    , -- , run Builtins.verifyBlsFunctionsTestInfo -- TODO: enable when PlutusV3 is supported again
      run Babbage.referenceScriptMintTestInfo
    , run Babbage.referenceScriptInlineDatumSpendTestInfo
    , run Babbage.referenceScriptDatumHashSpendTestInfo
    , run Babbage.inlineDatumSpendTestInfo
    , run Babbage.referenceInputWithV1ScriptErrorTestInfo
    , run Babbage.referenceScriptOutputWithV1ScriptErrorTestInfo
    , run Babbage.inlineDatumOutputWithV1ScriptErrorTestInfo
    , run Babbage.returnCollateralWithTokensValidScriptTestInfo
    , run Babbage.submitWithInvalidScriptThenCollateralIsTakenAndReturnedTestInfo
    ]

  failureMessages <- liftIO $ suiteFailureMessages resultsRef
  liftIO $ putStrLn $ "Number of test failures in suite: " ++ (show $ length failureMessages)
  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

-- uses short epochs for testing governance actions
pv9GovernanceTests :: IORef [TestResult] -> H.Property
pv9GovernanceTests resultsRef = integrationRetryWorkspace 0 "pv9Governance" $ \tempAbsPath -> do
  let options = TN.testnetOptionsConway9Governance
  preTestnetTime <- liftIO Time.getPOSIXTime
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
    TN.setupTestEnvironment options tempAbsPath
  let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
      run testInfo = runTest testInfo resultsRef options testParams

  -- generate DRep and staking keys and credentials to use in tests
  let ceo = toConwayEraOnwards $ TN.eraFromOptions options
  dRep <- liftIO $ generateDRepKeyCredentialsAndCertificate ceo
  staking <- liftIO $ generateStakeKeyCredentialAndCertificate ceo
  committee <- liftIO $ generateCommitteeKeysAndCertificate ceo

  sequence_
    [ run $ Conway.registerStakingTestInfo staking
    , run $ Conway.registerDRepTestInfo dRep
    , run $ Conway.delegateToDRepTestInfo dRep staking
    , run $ Conway.registerCommitteeTestInfo staking committee
    , run $ Conway.constitutionProposalAndVoteTestInfo dRep
    , run $ Conway.committeeProposalAndVoteTestInfo dRep committee
    ]

  failureMessages <- liftIO $ suiteFailureMessages resultsRef
  liftIO $ putStrLn $ "Number of test failures in suite: " ++ (show $ length failureMessages)
  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

debugTests :: IORef [TestResult] -> H.Property
debugTests resultsRef = integrationRetryWorkspace 0 "debug" $ \tempAbsPath -> do
  let options = TN.testnetOptionsAlonzo6
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
    TN.setupTestEnvironment options tempAbsPath
  let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath Nothing

  -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
  runTest Builtins.verifySchnorrAndEcdsaTestInfo resultsRef options testParams

  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

localNodeTests
  :: IORef [TestResult]
  -> Either (TN.LocalNodeOptions C.BabbageEra) (TN.TestnetOptions C.BabbageEra)
  -> H.Property
localNodeTests resultsRef options = integrationRetryWorkspace 0 "local" $ \tempAbsPath -> do
  -- preTestnetTime <- liftIO Time.getPOSIXTime
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
    TN.setupTestEnvironment options tempAbsPath
  let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath Nothing
      run name = runTest name resultsRef options testParams

  -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
  -- TODO: pass in or query for slot range to use in checkTxInfo tests
  -- runTestWithPosixTime "checkTxInfoV1Test" Alonzo.checkTxInfoV1Test options testParams preTestnetTime
  -- runTestWithPosixTime "checkTxInfoV2Test" Babbage.checkTxInfoV2Test options testParams preTestnetTime
  run Builtins.verifySchnorrAndEcdsaTestInfo
  run Babbage.referenceScriptMintTestInfo
  run Babbage.referenceScriptInlineDatumSpendTestInfo
  run Babbage.referenceScriptDatumHashSpendTestInfo

  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

runTestsWithResults :: IO ()
runTestsWithResults = do
  createDirectoryIfMissing False "test-report-xml"

  allRefs@[pv6ResultsRef, pv7ResultsRef, pv8ResultsRef, pv9ResultsRef, pv9GovResultsRef] <-
    traverse newIORef $ replicate 5 []

  -- Catch the exception returned by defaultMain to proceed with report generation
  eException <-
    try
      ( defaultMain $
          tests $
            ResultsRefs pv6ResultsRef pv7ResultsRef pv8ResultsRef pv9ResultsRef pv9GovResultsRef
      )
      :: IO (Either ExitCode ())

  [pv6Results, pv7Results, pv8Results, pv9Results, pv9GovResults] <- traverse readIORef allRefs

  failureMessages <- liftIO $ allFailureMessages allRefs
  liftIO $ putStrLn $ "Total number of test failures: " ++ (show $ length failureMessages)

  let pv6TestSuiteResult = TestSuiteResults "Alonzo PV6 Tests" pv6Results
      pv7TestSuiteResult = TestSuiteResults "Babbage PV7 Tests" pv7Results
      pv8TestSuiteResult = TestSuiteResults "Babbage PV8 Tests" pv8Results
      pv9TestSuiteResult = TestSuiteResults "Conway PV9 Tests" pv9Results
      pv9GovernanceTestSuiteResult = TestSuiteResults "Conway PV9 Governanace Tests" pv9GovResults

  -- Use 'results' to generate custom JUnit XML report
  let xml =
        testSuitesToJUnit
          [ pv6TestSuiteResult
          , pv7TestSuiteResult
          , pv8TestSuiteResult
          , pv9TestSuiteResult
          , pv9GovernanceTestSuiteResult
          ]
  writeFile "test-report-xml/test-results.xml" $ showTopElement xml

  when (eException /= Left ExitSuccess || length failureMessages > 0) exitFailure
