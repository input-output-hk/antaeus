{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.Committee where

import Cardano.Api qualified as C
import Cardano.Ledger.Keys qualified as Keys
import Control.Monad.IO.Class (MonadIO, liftIO)

data Committee era = Committee
  { committeeColdSKey :: C.SigningKey C.CommitteeColdKey
  , commiteeColdVKey :: C.VerificationKey C.CommitteeColdKey
  , committeeColdKeyHash :: C.Hash C.CommitteeColdKey
  , committeeHotSKey :: C.SigningKey C.CommitteeHotKey
  , -- , committeeHotVKey :: C.VerificationKey C.CommitteeHotKey
    committeeHotKeyAuthCert :: C.Certificate era
  }
  deriving (Show)

generateCommitteeKeysAndCertificate
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> m (Committee era)
generateCommitteeKeysAndCertificate ceo = do
  -- generate committee cold key
  committeeColdSKey <- liftIO $ C.generateSigningKey C.AsCommitteeColdKey
  let
    committeeColdVerificationKey@(C.CommitteeColdVerificationKey committeeColdVkey) =
      C.getVerificationKey committeeColdSKey
    committeeColdHash = C.verificationKeyHash committeeColdVerificationKey
  -- generate committee hot key
  committeeHotSKey <- liftIO $ C.generateSigningKey C.AsCommitteeHotKey
  let
    _committeeHotVerificationKey@(C.CommitteeHotVerificationKey committeeHotVKey) =
      C.getVerificationKey committeeHotSKey
    -- committeeHotHash = C.verificationKeyHash $ C.getVerificationKey committeeHotSKey

    ckh = C.conwayEraOnwardsConstraints ceo $ Keys.hashKey committeeColdVkey
    hkh = C.conwayEraOnwardsConstraints ceo $ Keys.hashKey committeeHotVKey
    committeeHotRequirements = C.CommitteeHotKeyAuthorizationRequirements ceo ckh hkh
    committeeHotKeyAuthCert = C.makeCommitteeHotKeyAuthorizationCertificate committeeHotRequirements
  return $
    Committee
      committeeColdSKey
      committeeColdVerificationKey
      committeeColdHash
      committeeHotSKey
      committeeHotKeyAuthCert
