{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LockingContract where


import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import Ledger.Address 
import           Ledger hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts 
import           Prelude (IO, Show (..), String)
import           Utilities            (writeValidatorToFile)

import           Common.Utils as U

-------------------------------
-- Locking Contract On-Chain --
-------------------------------

data LockingDatum = LD 
                    { metadata :: !BuiltinByteString
                    , count :: !Integer
                    , address  :: !PubKeyHash
                    , cost  :: !Integer
                    , beneficiary :: !PaymentPubKeyHash
                    , deadline :: !POSIXTime
                    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''LockingDatum

data LockingRedeemer = LR 
                       { upgrade :: !BuiltinByteString }

PlutusTx.unstableMakeIsData ''LockingRedeemer

{-# INLINABLE mkLockingValidator #-}
mkLockingValidator :: LockingDatum -> LockingRedeemer -> ScriptContext -> Bool
mkLockingValidator dat red ctx = traceIfFalse "Beneficiary Not Signed" signedByBeneficiary &&
                                -- traceIfFalse "Deadline Not Reached" deadlineReached
                                 traceIfFalse "Upgrade Failed" (validate ctx) 
    where

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy (U.info ctx) $ unPaymentPubKeyHash $ beneficiary dat

        validate :: ScriptContext -> Bool
        validate ctx = 
            validateTxOuts -- currently only validates txOut is to A SCRIPT & Contains Thread Token

        validateTxOuts :: Bool
        validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

        txOutValidate :: TxOut -> Bool
        txOutValidate txo = 
            checkOutput txo &&
            containsNewDatum txo 

        containsNewDatum :: TxOut -> Bool
        containsNewDatum txo =  
            findDatumHash' (expectedNewDatum $ LockingContract.upgrade red) (U.info ctx) == txOutDatumHash txo 

        checkOutput :: TxOut -> Bool
        checkOutput txo = length (flattenValue $ (txOutValue txo)) == 2

        findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash 
        findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

        expectedNewDatum :: BuiltinByteString -> LockingDatum
        expectedNewDatum x = LD
            { metadata = x 
            , count = LockingContract.count dat
            , address = LockingContract.address dat 
            , cost = LockingContract.cost dat
            , beneficiary = LockingContract.beneficiary dat
            , deadline = LockingContract.deadline dat
            }

data Locking
instance Scripts.ValidatorTypes Locking where
    type instance DatumType Locking = LockingDatum
    type instance RedeemerType Locking = LockingRedeemer

typedValidator :: Scripts.TypedValidator Locking
typedValidator = Scripts.mkTypedValidator @Locking
    $$(PlutusTx.compile [|| mkLockingValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @LockingDatum @LockingRedeemer

lockingValidator :: Validator
lockingValidator = Scripts.validatorScript typedValidator 

lockingHash :: Ledger.ValidatorHash 
lockingHash = Scripts.validatorHash typedValidator

lockingAddress :: Ledger.Address
lockingAddress = scriptAddress lockingValidator
