{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.V2TxInfo (
  txInfoInputs,
  txInfoOutputs,
  txInfoFee,
  txInfoMint,
  txInfoSigs,
  txInfoData,
  checkV2TxInfoScriptV2,
  checkV2TxInfoAssetIdV2,
  checkV2TxInfoRedeemer,
  checkV2TxInfoMintWitnessV2,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.Common (toShelleyBasedEra)
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import Helpers.TypeConverters (
  fromCardanoPaymentKeyHash,
  fromCardanoScriptData,
  fromCardanoTxIn,
  fromCardanoTxOutToPV2TxInfoTxOut,
  fromCardanoTxOutToPV2TxInfoTxOut',
  fromCardanoValue,
 )
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)
import PlutusScripts.Helpers (mintScriptWitness', plutusL2, policyIdV2, toScriptData)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins qualified as P
import PlutusTx.Prelude qualified as P

data V2TxInfo = V2TxInfo
  { expTxInfoInputs :: [PlutusV2.TxInInfo]
  -- ^ Transaction inputs; cannot be an empty list
  , expTxInfoReferenceInputs :: [PlutusV2.TxInInfo]
  -- ^ Added in V2: Transaction reference inputs
  , expTxInfoOutputs :: [PlutusV2.TxOut]
  -- ^ Transaction outputs
  , expTxInfoFee :: PlutusV2.Value
  -- ^ The fee paid by this transaction.
  , expTxInfoMint :: PlutusV2.Value
  -- ^ The 'Value' minted by this transaction.
  , expTxInfoDCert :: [PlutusV2.DCert]
  -- ^ Digests of certificates included in this transaction
  , expTxInfoWdrl :: PlutusV2.Map PlutusV2.StakingCredential Integer
  -- ^ Withdrawals
  , expTxInfoValidRange :: PlutusV2.POSIXTimeRange
  -- ^ The valid range for the transaction.
  , expTxInfoSignatories :: [PlutusV2.PubKeyHash]
  -- ^ Signatures provided with the transaction, attested that they all signed the tx
  , expTxInfoRedeemers :: PlutusV2.Map PlutusV2.ScriptPurpose PlutusV2.Redeemer
  -- ^ Added in V2: a table of redeemers attached to the transaction
  , expTxInfoData :: PlutusV2.Map PlutusV2.DatumHash PlutusV2.Datum
  -- ^ The lookup table of datums attached to the transaction
  -- , expTxInfoId          :: PlutusV2.TxId
  -- ^ Hash of the pending transaction body (i.e. transaction excluding witnesses). Cannot be verified onchain.
  }
PlutusTx.unstableMakeIsData ''V2TxInfo

checkV2TxInfoRedeemer
  :: [PlutusV2.TxInInfo]
  -> [PlutusV2.TxInInfo]
  -> [PlutusV2.TxOut]
  -> PlutusV2.Value
  -> PlutusV2.Value
  -> [PlutusV2.DCert]
  -> PlutusV2.Map PlutusV2.StakingCredential Integer
  -> PlutusV2.POSIXTimeRange
  -> [PlutusV2.PubKeyHash]
  -> PlutusV2.Map PlutusV2.ScriptPurpose PlutusV2.Redeemer
  -> PlutusV2.Map PlutusV2.DatumHash PlutusV2.Datum
  -> C.HashableScriptData
checkV2TxInfoRedeemer expIns expRefIns expOuts expFee expMint expDCert expWdrl expRange expSigs expReds expData =
  toScriptData $
    V2TxInfo expIns expRefIns expOuts expFee expMint expDCert expWdrl expRange expSigs expReds expData

txInfoInputs :: C.CardanoEra era -> (C.TxIn, C.TxOut C.CtxUTxO era) -> PlutusV2.TxInInfo
txInfoInputs era (txIn, txOut) = do
  PlutusV2.TxInInfo
    { PlutusV2.txInInfoOutRef = fromCardanoTxIn txIn
    , PlutusV2.txInInfoResolved = fromCardanoTxOutToPV2TxInfoTxOut' (toShelleyBasedEra era) txOut
    }

txInfoOutputs :: C.CardanoEra era -> [C.TxOut C.CtxTx era] -> [PlutusV2.TxOut]
txInfoOutputs era = map (fromCardanoTxOutToPV2TxInfoTxOut (toShelleyBasedEra era))

txInfoFee :: C.Lovelace -> PlutusV2.Value
txInfoFee = fromCardanoValue . C.lovelaceToValue

txInfoMint :: C.Value -> PlutusV2.Value
txInfoMint = fromCardanoValue

txInfoSigs :: [C.VerificationKey C.PaymentKey] -> [PlutusV2.PubKeyHash]
txInfoSigs = map (fromCardanoPaymentKeyHash . C.verificationKeyHash)

txInfoData :: [C.HashableScriptData] -> PlutusV2.Map PlutusV2.DatumHash PlutusV2.Datum
txInfoData =
  PlutusV2.fromList
    . map
      ( \datum ->
          ( PlutusV2.DatumHash $ PlutusV2.toBuiltin $ C.serialiseToRawBytes $ C.hashScriptDataBytes datum
          , PlutusV2.Datum $ fromCardanoScriptData datum
          )
      )

-- minting policy --

{-# INLINEABLE mkCheckV2TxInfo #-}
mkCheckV2TxInfo :: V2TxInfo -> PlutusV2.ScriptContext -> Bool
mkCheckV2TxInfo V2TxInfo{..} ctx =
  P.traceIfFalse "unexpected txInfoInputs" checkTxInfoInputs
    P.&& P.traceIfFalse "unexpected txInfoReferenceInputs" checkTxInfoReferenceInputs
    P.&& P.traceIfFalse "unexpected txInfoOutputs" checkTxInfoOutputs
    P.&& P.traceIfFalse "unexpected txInfoFee" checkTxInfoFee
    P.&& P.traceIfFalse "unexpected txInfoMint" checkTxInfoMint
    P.&& P.traceIfFalse "unexpected txInfoDCert" checkTxInfoDCert
    P.&& P.traceIfFalse "unexpected txInfoWdrl" checkTxInfoWdrl
    P.&& P.traceIfFalse "provided range doesn't contain txInfoValidRange" checkTxInfoValidRange
    P.&& P.traceIfFalse "unexpected txInfoSignatories" checkTxInfoSignatories
    P.&& P.traceIfFalse "unexpected txInfoRedeemers" checkTxInfoRedeemers
    P.&& P.traceIfFalse "unexpected txInfoData" checkTxInfoData
    P.&& P.traceIfFalse "txInfoId isn't the expected TxId length" checkTxInfoId
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    checkTxInfoInputs = expTxInfoInputs P.== PlutusV2.txInfoInputs info
    checkTxInfoReferenceInputs = expTxInfoReferenceInputs P.== PlutusV2.txInfoReferenceInputs info
    checkTxInfoOutputs = expTxInfoOutputs P.== PlutusV2.txInfoOutputs info
    checkTxInfoFee = expTxInfoFee P.== PlutusV2.txInfoFee info
    checkTxInfoMint = expTxInfoMint P.== PlutusV2.txInfoMint info
    checkTxInfoDCert = expTxInfoDCert P.== PlutusV2.txInfoDCert info
    checkTxInfoWdrl = expTxInfoWdrl P.== PlutusV2.txInfoWdrl info
    checkTxInfoValidRange = expTxInfoValidRange `P.contains` PlutusV2.txInfoValidRange info
    checkTxInfoSignatories = expTxInfoSignatories P.== PlutusV2.txInfoSignatories info
    checkTxInfoRedeemers = do
      let ownScriptPurpose = PlutusV2.Minting (ownCurrencySymbol ctx)
          withoutOwnRedeemer = AMap.delete ownScriptPurpose (PlutusV2.txInfoRedeemers info)
      expTxInfoRedeemers P.== withoutOwnRedeemer -- cannot check own redeemer so only check other script's redeemer
    checkTxInfoData = expTxInfoData P.== PlutusV2.txInfoData info
    checkTxInfoId = P.equalsInteger 32 (P.lengthOfByteString P.$ PlutusV2.getTxId P.$ PlutusV2.txInfoId info)

checkV2TxInfoV2 :: SerialisedScript
checkV2TxInfoV2 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkCheckV2TxInfo

checkV2TxInfoScriptV2 :: C.PlutusScript C.PlutusScriptV2
checkV2TxInfoScriptV2 = C.PlutusScriptSerialised checkV2TxInfoV2

checkV2TxInfoAssetIdV2 :: C.AssetId
checkV2TxInfoAssetIdV2 = C.AssetId (policyIdV2 checkV2TxInfoV2) "V2TxInfo"

checkV2TxInfoMintWitnessV2
  :: C.ShelleyBasedEra era
  -> C.HashableScriptData
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkV2TxInfoMintWitnessV2 sbe redeemer exunits =
  ( policyIdV2 checkV2TxInfoV2
  , mintScriptWitness' sbe plutusL2 (Left checkV2TxInfoScriptV2) redeemer exunits
  )
