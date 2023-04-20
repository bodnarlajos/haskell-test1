{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestMonad where

import Control.Monad.Trans.Reader (ReaderT)

newtype TestMonad a = TestMonad
  { runTestMonad :: ReaderT Int IO a
  }
  deriving (Functor, Applicative, Monad)
