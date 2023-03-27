{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingVia          #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- We suffix the record fields with ' to denote these are Okapi fields, and we provide accessor functions of the same name (wo the suffix) that unOkify these fields.
module Okapi.UntypedOkapiContexts
  ( OkScriptContext (..),
    okInfo,
    scriptContextPurpose,
    OkTxInfo (..),
    txInfoInputs,
    txInfoOutputs,
    txInfoFee,
    txInfoMint,
    txInfoDCert,
    txInfoWdrl,
    txInfoValidRange,
    txInfoSignatories,
    txInfoData,
    txInfoId,
    OkScriptPurpose (..),
    ownCurrencySymbol,
    OkTxOut (..),
    txOutAddress,
    txOutValue,
    txOutDatumHash,
    OkTxInInfo (..),
    txInInfoOutRef,
    txInInfoResolved,
    spendsOutput,
    findDatum,
    findOwnInput,
  )
where

import GHC.Generics (Generic)
import Okapi.Untyped (Ok, unOk)
import Plutus.V2.Ledger.Api
  ( Address,
    CurrencySymbol,
    DCert,
    Datum,
    DatumHash,
    POSIXTimeRange,
    PubKeyHash,
    StakingCredential,
    TxId,
    TxOutRef,
    Value,
    txOutRefId,
    txOutRefIdx,
  )
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Prelude
  ( Bool (False),
    Eq,
    Integer,
    Maybe (Nothing),
    any,
    error,
    find,
    snd,
    (&&),
    (.),
    (<$>),
    (==),
  )
import qualified Prelude as Haskell

data OkScriptPurpose
  = Minting (Ok CurrencySymbol)
  | Spending (Ok TxOutRef)
  | Rewarding (Ok StakingCredential)
  | Certifying (Ok DCert)
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

data OkScriptContext = OkScriptContext
  { scriptContextTxInfo' :: Ok OkTxInfo,
    scriptContextPurpose' :: Ok OkScriptPurpose
  }
  deriving stock (Generic, Haskell.Eq)

{-# INLINEABLE okInfo #-}
okInfo :: OkScriptContext -> OkTxInfo
okInfo = unOk . scriptContextTxInfo'

{-# INLINEABLE scriptContextPurpose #-}
scriptContextPurpose :: OkScriptContext -> OkScriptPurpose
scriptContextPurpose = unOk . scriptContextPurpose'

{-# INLINEABLE ownCurrencySymbol #-}
ownCurrencySymbol :: OkScriptContext -> CurrencySymbol
ownCurrencySymbol context =
  let purpose = scriptContextPurpose context
   in case purpose of
        Minting cs -> unOk cs
        _ -> error ()

data OkTxOut = OkTxOut
  { txOutAddress' :: Ok Address,
    txOutValue' :: Ok Value,
    txOutDatumHash' :: Ok (Maybe DatumHash)
  }
  deriving stock (Haskell.Eq, Generic)

txOutAddress :: OkTxOut -> Address
txOutAddress = unOk . txOutAddress'

txOutValue :: OkTxOut -> Value
txOutValue = unOk . txOutValue'

txOutDatumHash :: OkTxOut -> Maybe DatumHash
txOutDatumHash = unOk . txOutDatumHash'

-- | An input of a pending transaction.
data OkTxInInfo = OkTxInInfo
  { txInInfoOutRef' :: Ok TxOutRef,
    txInInfoResolved' :: Ok OkTxOut
  }
  deriving stock (Generic, Haskell.Eq)

txInInfoOutRef :: OkTxInInfo -> TxOutRef
txInInfoOutRef = unOk . txInInfoOutRef'

txInInfoResolved :: OkTxInInfo -> OkTxOut
txInInfoResolved = unOk . txInInfoResolved'

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data OkTxInfo = OkTxInfo
  { -- | Transaction inputs
    txInfoInputs' :: Ok [OkTxInInfo],
    -- | Transaction outputs
    txInfoOutputs' :: Ok [OkTxOut],
    -- | The fee paid by this transaction.
    txInfoFee' :: Ok Value,
    -- | The 'Value' minted by this transaction.
    txInfoMint' :: Ok Value,
    -- | Digests of certificates included in this transaction
    txInfoDCert' :: Ok [DCert],
    -- | Withdrawals
    txInfoWdrl' :: Ok [(StakingCredential, Integer)],
    -- | The valid range for the transaction.
    txInfoValidRange' :: Ok POSIXTimeRange,
    -- | Signatures provided with the transaction, attested that they all signed the tx
    txInfoSignatories' :: Ok [PubKeyHash],
    txInfoData' :: Ok [(DatumHash, Datum)],
    -- | Hash of the pending transaction (excluding witnesses)
    txInfoId' :: Ok TxId
  }
  deriving stock (Generic, Haskell.Eq)

{-# INLINEABLE txInfoInputs #-}
txInfoInputs :: OkTxInfo -> [OkTxInInfo]
txInfoInputs = unOk . txInfoInputs'

{-# INLINEABLE txInfoOutputs #-}
txInfoOutputs :: OkTxInfo -> [OkTxOut]
txInfoOutputs = unOk . txInfoOutputs'

{-# INLINEABLE txInfoFee #-}
txInfoFee :: OkTxInfo -> Value
txInfoFee = unOk . txInfoFee'

{-# INLINEABLE txInfoMint #-}
txInfoMint :: OkTxInfo -> Value
txInfoMint = unOk . txInfoMint'

{-# INLINEABLE txInfoDCert #-}
txInfoDCert :: OkTxInfo -> [DCert]
txInfoDCert = unOk . txInfoDCert'

{-# INLINEABLE txInfoWdrl #-}
txInfoWdrl :: OkTxInfo -> [(StakingCredential, Integer)]
txInfoWdrl = unOk . txInfoWdrl'

{-# INLINEABLE txInfoValidRange #-}
txInfoValidRange :: OkTxInfo -> POSIXTimeRange
txInfoValidRange = unOk . txInfoValidRange'

{-# INLINEABLE txInfoSignatories #-}
txInfoSignatories :: OkTxInfo -> [PubKeyHash]
txInfoSignatories = unOk . txInfoSignatories'

{-# INLINEABLE txInfoData #-}
txInfoData :: OkTxInfo -> [(DatumHash, Datum)]
txInfoData = unOk . txInfoData'

{-# INLINEABLE txInfoId #-}
txInfoId :: OkTxInfo -> TxId
txInfoId = unOk . txInfoId'

{-# INLINEABLE spendsOutput #-}

-- | Check if the pending transaction spends a specific transaction output
--   (identified by the hash of a transaction and an index into that
--   transactions' outputs)
spendsOutput :: OkTxInfo -> TxId -> Integer -> Bool
spendsOutput p h i =
  let spendsOutRef inp =
        let outRef = txInInfoOutRef inp
         in h == txOutRefId outRef
              && i == txOutRefIdx outRef
   in any spendsOutRef (txInfoInputs p)

{-# INLINEABLE findDatum #-}

-- | Find the data corresponding to a data hash, if there is one
findDatum :: DatumHash -> OkTxInfo -> Maybe Datum
findDatum dsh info = snd <$> find f (txInfoData info)
  where
    f (dsh', _) = dsh' == dsh

{-# INLINEABLE findOwnInput #-}

-- | Find the input currently being validated.
findOwnInput :: OkScriptContext -> Maybe OkTxInInfo
findOwnInput context =
  let info = okInfo context
      inputs = txInfoInputs info
   in case scriptContextPurpose context of
        Spending txOutRef -> find (\input -> txInInfoOutRef input == unOk txOutRef) inputs
        _ -> Nothing

instance Eq OkTxOut where
  {-# INLINEABLE (==) #-}
  OkTxOut a v dh == OkTxOut a' v' dh' =
    a == a' && v == v' && dh == dh'

instance Eq OkTxInInfo where
  OkTxInInfo ref res == OkTxInInfo ref' res' =
    ref == ref'
      && res == res'

instance Eq OkTxInfo where
  {-# INLINEABLE (==) #-}
  OkTxInfo i o f m c w r s d tid == OkTxInfo i' o' f' m' c' w' r' s' d' tid' =
    i == i'
      && o == o'
      && f == f'
      && m == m'
      && c == c'
      && w == w'
      && r == r'
      && s == s'
      && d == d'
      && tid == tid'

instance Eq OkScriptPurpose where
  {-# INLINEABLE (==) #-}
  Minting cs == Minting cs' = cs == cs'
  Spending ref == Spending ref' = ref == ref'
  Rewarding sc == Rewarding sc' = sc == sc'
  Certifying cert == Certifying cert' = cert == cert'
  _ == _ = False

PlutusTx.makeIsDataIndexed ''OkTxOut [('OkTxOut, 0)]
PlutusTx.makeIsDataIndexed ''OkTxInInfo [('OkTxInInfo, 0)]

-- PlutusTx.makeLift ''TxInfo
PlutusTx.makeIsDataIndexed ''OkTxInfo [('OkTxInfo, 0)]

-- PlutusTx.makeLift ''SpookyScriptPurpose
PlutusTx.makeIsDataIndexed
  ''OkScriptPurpose
  [ ('Minting, 0),
    ('Spending, 1),
    ('Rewarding, 2),
    ('Certifying, 3)
  ]

-- -- PlutusTx.makeLift ''SpookyScriptContext
PlutusTx.makeIsDataIndexed ''OkScriptContext [('OkScriptContext, 0)]
