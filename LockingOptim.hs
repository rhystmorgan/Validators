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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module LockingContract where


import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           Ledger.Value
import           Ledger.Address  
import           Prelude (IO, Show (..), String)
import           Okapi.Untyped as Ok
import           Okapi.UntypedOkapiContexts as Ok
import           Common.Utils as U
import           Utilities            (writeValidatorToFile)

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
mkLockingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkLockingValidator dat' red' ctx' 
    | signedByBeneficiary && (validate ctx) = ()
    | otherwise                             = traceError "Upgrade Failed"
                                
    where
        ctx :: OkScriptContext 
        ctx = (unsafeFromBuiltinData ctx')

        dat :: LockingDatum 
        dat = (unsafeFromBuiltinData dat')

        red :: LockingRedeemer 
        red = (unsafeFromBuiltinData red')

        signedByBeneficiary :: Bool
        signedByBeneficiary = elem (unPaymentPubKeyHash $ beneficiary dat) (Ok.txInfoSignatories $ okInfo ctx)

        validate :: OkScriptContext -> Bool
        validate ctx = 
            validateTxOuts 

        validateTxOuts :: Bool
        validateTxOuts = any txOutValidate (Ok.txInfoOutputs $ okInfo ctx)

        txOutValidate :: OkTxOut -> Bool
        txOutValidate txo = 
            checkOutput txo &&
            containsNewDatum txo 

        containsNewDatum :: OkTxOut -> Bool
        containsNewDatum txo =  
            findDatumHash' (expectedNewDatum $ upgrade red) (okInfo ctx) == Ok.txOutDatumHash txo 

        checkOutput :: OkTxOut -> Bool
        checkOutput txo = length (flattenValue $ (Ok.txOutValue txo)) == 2

        findDatumHash' :: Plutus.V2.Ledger.Api.ToData a => a -> OkTxInfo -> Maybe DatumHash 
        findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) (unOk (txInfoData' info))

        expectedNewDatum :: BuiltinByteString -> LockingDatum
        expectedNewDatum x = LD
            { metadata = x 
            , count = LockingContract.count dat
            , address = LockingContract.address dat 
            , cost = LockingContract.cost dat
            , beneficiary = LockingContract.beneficiary dat
            , deadline = LockingContract.deadline dat
            }


lockingValidator :: Validator
lockingValidator = mkValidatorScript $$(PlutusTx.compile [|| mkLockingValidator ||])
