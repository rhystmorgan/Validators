{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingVia          #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Okapi.Untyped
  ( Ok,
    toOk,
    unOk
  )
where

import PlutusTx
  ( BuiltinData,
    ToData,
    UnsafeFromData,
    toBuiltinData,
    unsafeFromBuiltinData,
  )

type Ok a = BuiltinData

{-# INLINEABLE unOk #-}
unOk ::
  UnsafeFromData a =>
  Ok a ->
  a
unOk = unsafeFromBuiltinData

toOk ::
  ToData a =>
  a ->
  Ok a
toOk = toBuiltinData
{-# INLINE toOk #-}
