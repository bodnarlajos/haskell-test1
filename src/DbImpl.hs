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
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.String (IsString(fromString))
import Control.Exception (bracket, SomeException (SomeException), catch)
import Control.Monad.Trans.Class (lift)

type Program = LoggingT (ReaderT DI IO)

runProgram :: Program Text -> DI -> IO Text
runProgram worker = runReaderT (runStdoutLoggingT worker)

instance Printer Program where
  print = liftIO . Prelude.print

instance Db Program where
  readDb :: DbQuery -> Program [DbRow]
  readDb queryStr = do
    sqlitePath <- connStr <$> lift ask
    let q = fromString (unpack queryStr) :: Query
    liftIO $ catch (bracket (open sqlitePath) close
      (\conn -> query_ conn q :: IO [DbRow])) (\(SomeException e) -> Prelude.print e >> return [] :: IO [DbRow])
  writeDb :: DbQuery -> Program ()
  writeDb queryStr = do
    sqlitePath <- connStr <$> lift ask
    let q = fromString (unpack queryStr) :: Query
    liftIO $ catch (bracket (open sqlitePath) close
      (\conn -> execute_ conn q)) (\(SomeException e) -> Prelude.print e)
