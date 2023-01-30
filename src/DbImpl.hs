{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DbImpl where

import Db
import DbEntities
import Database.SQLite.Simple
import Data.Text (unpack, Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.String (IsString(fromString))

type Program m a = (Db m, Printer m, Monad m, Functor m, Applicative m) => LoggingT (ReaderT DI m) a

runProgram :: Program IO Text -> DI -> IO Text
runProgram worker = runReaderT (runStdoutLoggingT worker)

instance Printer IO where
  print = Prelude.print

instance Db IO where
  readDb :: DbQuery -> IO [DbRow]
  readDb queryStr = do
    sqlitePath <- getConfig
    let sqlitePath' = dbPath sqlitePath
    conn <- liftIO $ open sqlitePath'
    let q = fromString (unpack queryStr) :: Query
    res <- liftIO $ query_ conn q :: IO [DbRow]
    liftIO $ close conn 
    return res
  writeDb _ = undefined
  getConfig = return $ DbConfig "/home/lajbo/Projects/haskell-projects/test1/db.sqlite"
