{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Db where

import Prelude (Applicative, Semigroup ((<>)))
import Control.Monad (Functor, Monad)
import Text.Show ( Show )
import Data.Eq ( Eq )
import Data.Typeable (Typeable)

newtype Db a = Db a deriving newtype (Eq, Show, Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (Db a) where
  (<>) :: Db a -> Db a -> Db a
  (Db a) <> (Db b) =  Db (a <> b)

-- instance Applicative Db where
--   pure :: a -> Db a
--   pure = Db
--   (<*>) :: Db (a -> b) -> Db a -> Db b
--   (<*>) (Db fa) (Db a) = Db (fa a)