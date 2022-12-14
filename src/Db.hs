{-# LANGUAGE InstanceSigs #-}

module Db where

import Prelude (Semigroup ((<>)), Monad ((>>=)))
import Control.Monad (Functor, Monad (return))
import Text.Show ( Show )
import Data.Eq ( Eq )
import Control.Applicative (Applicative, pure, (<*>))
import Data.Functor (fmap)

newtype Db a = Db a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Db a) where
  (<>) :: Db a -> Db a -> Db a
  (Db a) <> (Db b) =  Db (a <> b)

instance Functor Db where
  fmap :: (a -> b) -> Db a -> Db b
  fmap f (Db a) = Db (f a)

instance Applicative Db where
  pure :: a -> Db a
  pure = Db
  (<*>) :: Db (a -> b) -> Db a -> Db b
  (<*>) (Db fa) (Db a) = Db (fa a)

instance Monad Db where
  return :: a -> Db a
  return = Db
  (>>=) :: Db a -> (a -> Db b) -> Db b
  (Db a) >>= fv = fv a