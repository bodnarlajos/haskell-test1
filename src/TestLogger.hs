{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestLogger where

import Control.Monad.Logger
    ( runStdoutLoggingT, LoggingT )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Db
import DbImpl
import Control.Monad.Trans.Class (lift)
import Database.SQLite.Simple
import Data.Text (Text)


