{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DataKinds          #-}

module Helpers.Testnet where

import Cardano.Api (Error)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust)
import Hedgehog (MonadTest)
import Hedgehog.Extras.Stock (waitSecondsForProcess)
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Stock.OS qualified as OS
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Helpers.Common (cardanoEraToShelleyBasedEra, makeAddress, toEraInCardanoMode)
import Helpers.Utils (maybeReadAs)
import System.Directory qualified as IO

import System.FilePath ((</>))

#if defined(mingw32_HOST_OS)
  -- do no process kill signalling on windows
#else
import System.Posix.Signals (sigKILL, signalProcess)
#endif

import Cardano.Testnet qualified as C
import Cardano.Testnet qualified as CTN hiding (testnetMagic)
import Hedgehog qualified as H
import System.Process (cleanupProcess)
import System.Process.Internals (PHANDLE, ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle), withProcessHandle)
import Testnet.Util.Runtime qualified as CTN

data TestnetOptions = TestnetOptions
  { testnetEra             :: C.AnyCardanoEra
  , testnetProtocolVersion :: Int
  , testnetCardanoOptions  :: C.TestnetOptions
  }

defAlonzoTestnetOptions :: TestnetOptions
defAlonzoTestnetOptions = TestnetOptions
  { testnetEra = C.AnyCardanoEra C.AlonzoEra
  , testnetProtocolVersion = 6
  , testnetCardanoOptions = CTN.CardanoOnlyTestnetOptions CTN.cardanoDefaultTestnetOptions
      { CTN.cardanoEpochLength = 10_000 } -- higher value so that txs can have higher upper bound validity range
  }

defBabbageTestnetOptions :: Int -> TestnetOptions
defBabbageTestnetOptions protocolVersion = TestnetOptions
  { testnetEra = C.AnyCardanoEra C.BabbageEra
  , testnetProtocolVersion = protocolVersion
  , testnetCardanoOptions = C.BabbageOnlyTestnetOptions CTN.babbageDefaultTestnetOptions
      { CTN.babbageProtocolVersion = protocolVersion
      , CTN.babbageSlotDuration = 200
      , CTN.babbageEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
      }
  }

defConwayTestnetOptions :: TestnetOptions
defConwayTestnetOptions = TestnetOptions
  { testnetEra = C.AnyCardanoEra C.ConwayEra
  , testnetProtocolVersion = 9
  , testnetCardanoOptions = C.ConwayOnlyTestnetOptions CTN.babbageDefaultTestnetOptions
      { CTN.babbageProtocolVersion = 9
      , CTN.babbageSlotDuration = 200
      , CTN.babbageEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
      }
  }

data LocalNodeOptions = LocalNodeOptions
  { localNodeEra             :: C.AnyCardanoEra
  , localNodeProtocolVersion :: Int
  , localNodeEnvDir          :: FilePath -- path to directory containing 'utxo-keys' and 'ipc' directories
  , localNodeTestnetMagic    :: Int
  } deriving Show

localNodeOptionsPreview :: Either LocalNodeOptions TestnetOptions
localNodeOptionsPreview = Left $ LocalNodeOptions
  { localNodeEra = C.AnyCardanoEra C.BabbageEra
  , localNodeProtocolVersion = 8
  , localNodeEnvDir = "/tmp/preview"
  , localNodeTestnetMagic = 2
  }

data TimedOut = ProcessExitTimedOut Int PHANDLE deriving Show

instance Error TimedOut where
  displayError (ProcessExitTimedOut t pid) = "Timeout. Waited " ++ show t ++
                                             "s in `cleanupTestnet` for process to exit. pid=" ++ show pid

testnetOptionsAlonzo6, testnetOptionsBabbage7, testnetOptionsBabbage8, testnetOptionsConway9
  :: Either LocalNodeOptions TestnetOptions
testnetOptionsAlonzo6 = Right defAlonzoTestnetOptions
testnetOptionsBabbage7 = Right $ defBabbageTestnetOptions 7
testnetOptionsBabbage8 = Right $ defBabbageTestnetOptions 8
testnetOptionsConway9 = Right defConwayTestnetOptions

eraFromOptions :: (MonadTest m) => Either LocalNodeOptions TestnetOptions -> m C.AnyCardanoEra
eraFromOptions = return . either localNodeEra testnetEra

pvFromOptions :: (MonadTest m) => Either LocalNodeOptions TestnetOptions -> m Int
pvFromOptions = return . either localNodeProtocolVersion testnetProtocolVersion

-- | Get path to where cardano-testnet files are
getProjectBase :: (MonadIO m, MonadTest m) => m String
getProjectBase = liftIO . IO.canonicalizePath =<< HE.getProjectBase

-- | Start a testnet with provided testnet options (including era and protocol version)
startTestnet ::
  C.CardanoEra era ->
  TestnetOptions ->
  FilePath ->
  FilePath ->
  H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId, Maybe [CTN.PoolNode])
startTestnet era testnetOptions base tempAbsBasePath' = do
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf :: CTN.Conf <- HE.noteShowM $
    CTN.mkConf (CTN.ProjectBase base) (Just $ CTN.YamlFilePath configurationTemplate) (tempAbsBasePath' <> "/") Nothing
  tn <- CTN.testnet (testnetCardanoOptions testnetOptions) conf

  -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
  -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
  -- to the Byron era.
  socketPathAbs <- getPoolSocketPathAbs conf tn
  let epochSlots = C.EpochSlots 21_600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = getNetworkId tn
          , C.localNodeSocketPath = socketPathAbs
          }
      networkId = getNetworkId tn
  pparams <- getProtocolParams era localNodeConnectInfo
  -- no longer set node socket environment for Cardano.Api.Convenience.Query
  -- TODO: REMOVE: liftIO $ IO.setEnv "CARDANO_NODE_SOCKET_PATH" socketPathAbs
  pure (localNodeConnectInfo, pparams, networkId, Just $ CTN.poolNodes tn)

cleanupTestnet :: (MonadIO m) => Maybe [CTN.PoolNode] -> m [Either TimedOut ()]
cleanupTestnet mPoolNodes = case mPoolNodes of
    Just poolNodes -> do
      liftIO (mapM_ (\ (CTN.PoolNode poolRuntime _) ->
         -- graceful SIGTERM all nodes
        cleanupProcess (Just (CTN.nodeStdinHandle poolRuntime), Nothing, Nothing, CTN.nodeProcessHandle poolRuntime))
        poolNodes)
      if not OS.isWin32 then -- do no process kill signalling on windows
        -- kill signal for any node unix handles still open
        liftIO $ mapM (\node -> killUnixHandle $ CTN.nodeProcessHandle $ CTN.poolRuntime node) poolNodes
        else return []
    _ ->     return []
    where
      killUnixHandle ph = liftIO $ withProcessHandle ph $ \case
          OpenHandle pid    -> do
            signalProcess sigKILL pid -- send kill signal if handle still open
            eTimeOut <- waitSecondsForProcess 60 ph  -- wait 60s for process to exit
            case eTimeOut of
                Left _  -> return $ Left $ ProcessExitTimedOut 60 pid
                Right _ -> return $ Right ()
          OpenExtHandle _ _ -> return $ Right () -- do nothing on Windows
          ClosedHandle _    -> return $ Right () -- do nothing if already closed

connectToLocalNode ::
  C.CardanoEra era ->
  LocalNodeOptions ->
  FilePath ->
  H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId, Maybe [CTN.PoolNode])
connectToLocalNode era localNodeOptions tempAbsPath = do
  let localEnvDir' = localNodeEnvDir localNodeOptions

  HE.createDirectoryIfMissing (tempAbsPath </> "utxo-keys")
  HE.createDirectoryIfMissing (tempAbsPath </> "sockets")

  HE.createFileLink (localEnvDir' </> "test.skey") (tempAbsPath </> "utxo-keys/utxo1.skey")
  HE.createFileLink (localEnvDir' </> "test.vkey") (tempAbsPath </> "utxo-keys/utxo1.vkey")
  HE.createFileLink (localEnvDir' </> "ipc/node.socket") (tempAbsPath </> "sockets/node.socket")

  let socketPathAbs = C.File $ tempAbsPath </> "sockets/node.socket"
      networkId = C.Testnet $ C.NetworkMagic $ fromIntegral (localNodeTestnetMagic localNodeOptions)

  -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
  -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
  -- to the Byron era.
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots,
            C.localNodeNetworkId = networkId,
            C.localNodeSocketPath = socketPathAbs
          }
  pparams <- getProtocolParams era localNodeConnectInfo
  -- No longer set node socket environment for Cardano.Api.Convenience.Query
  --TODO: REMOVE: liftIO $ IO.setEnv "CARDANO_NODE_SOCKET_PATH" socketPathAbs
  pure (localNodeConnectInfo, pparams, networkId, Nothing)

-- | Start testnet with cardano-testnet or use local node that's already
--   connected to a public testnet
setupTestEnvironment ::
  Either LocalNodeOptions TestnetOptions ->
  FilePath ->
  H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId, Maybe [CTN.PoolNode])
setupTestEnvironment options tempAbsPath = do
  case options of
    Left localNodeOptions -> do
      C.AnyCardanoEra era <- return $ localNodeEra localNodeOptions
      liftIO $ putStrLn "\nConnecting to local node..."
      connectToLocalNode era localNodeOptions tempAbsPath
    Right testnetOptions -> do
      C.AnyCardanoEra era <- return $ testnetEra testnetOptions
      pv <- pvFromOptions options
      base <- getProjectBase
      liftIO $ putStrLn $ "\nStarting local testnet in " ++ show era ++ " PV" ++ show pv ++ "..."
      startTestnet era testnetOptions base tempAbsPath

-- | Network ID of the testnet
getNetworkId :: CTN.TestnetRuntime -> C.NetworkId
getNetworkId tn = C.Testnet $ C.NetworkMagic $ fromIntegral (CTN.testnetMagic tn)

-- | Path to a pool node's unix socket
getPoolSocketPathAbs :: (MonadTest m, MonadIO m) => CTN.Conf -> CTN.TestnetRuntime -> m C.SocketPath
getPoolSocketPathAbs conf tn = do
  let tempAbsPath = CTN.tempAbsPath conf
  socketPath <- IO.sprocketArgumentName <$> H.headM (CTN.poolSprockets tn)
  fp <- liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath
  H.annotate fp
  return $ C.File fp

-- | Query network's protocol parameters
getProtocolParams :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> m C.ProtocolParameters
getProtocolParams era localNodeConnectInfo =
  H.leftFailM . H.leftFailM . liftIO $
    C.queryNodeLocalState localNodeConnectInfo Nothing $
      C.QueryInEra (toEraInCardanoMode era) $
      C.QueryInShelleyBasedEra (cardanoEraToShelleyBasedEra era) C.QueryProtocolParameters

-- | Signing key and address for wallet 1
--   Handles two key types: GenesisUTxOKey and PaymentKey
w1 ::
  (MonadIO m, MonadTest m) =>
  Either LocalNodeOptions TestnetOptions ->
  FilePath ->
  C.NetworkId ->
  m (C.SigningKey C.PaymentKey, C.VerificationKey C.PaymentKey, C.Address C.ShelleyAddr)
w1 networkOptions tempAbsPath' networkId = do
  let extendedPath = case networkOptions of -- because Testnet.Cardano uses slightly different filepath
        Right testnetOptions | CTN.CardanoOnlyTestnetOptions _ <- testnetCardanoOptions testnetOptions -> "shelley"
        _                                                                                              -> ""
  -- GenesisUTxOKey comes from cardano-testnet
  mGenesisVKey :: Maybe (C.VerificationKey C.GenesisUTxOKey) <-
    maybeReadAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ C.File $
      tempAbsPath' </> extendedPath </> "utxo-keys/utxo1.vkey"
  mGenesisSKey :: Maybe (C.SigningKey C.GenesisUTxOKey) <-
    maybeReadAs (C.AsSigningKey C.AsGenesisUTxOKey) $ C.File $ tempAbsPath' </> extendedPath </> "utxo-keys/utxo1.skey"
  -- PaymentKey comes from cardano-cli (the likely type for a locally created wallet)
  mPaymentVKey :: Maybe (C.VerificationKey C.PaymentKey) <-
    maybeReadAs (C.AsVerificationKey C.AsPaymentKey) $ C.File $ tempAbsPath' </> extendedPath </> "utxo-keys/utxo1.vkey"
  mPaymentSKey :: Maybe (C.SigningKey C.PaymentKey) <-
    maybeReadAs (C.AsSigningKey C.AsPaymentKey) $ C.File $ tempAbsPath' </> extendedPath </> "utxo-keys/utxo1.skey"

  let vKey :: C.VerificationKey C.PaymentKey = maybe (fromJust mPaymentVKey) C.castVerificationKey mGenesisVKey
      sKey :: C.SigningKey C.PaymentKey = maybe (fromJust mPaymentSKey) C.castSigningKey mGenesisSKey
      address = makeAddress (Left vKey) networkId

  return (sKey, vKey, address)
