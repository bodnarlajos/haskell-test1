{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module DbImpl where

import Db
import DbEntities
import Database.SQLite.Simple
import Data.Text (unpack, Text, pack, empty)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logDebugN)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.String (IsString(fromString))
import Control.Exception (bracket, SomeException (SomeException), catch, Exception (displayException), IOException)
import Control.Monad.Trans.Class (lift)
import RepositoryC (RepositoryC (getData), DataId, Data (Data))

type ProgramC m = (Db m, Printer m, RepositoryC m)
type Program = LoggingT (ReaderT DI IO)

runProgram :: Program a -> DI -> IO a
runProgram worker = runReaderT (runStdoutLoggingT worker)

instance Printer Program where
  print = logDebugN
  printRows = mapM_ (\(DbRow rowId txt) -> Db.print $ "id: " <> pack (show rowId) <> ", txt: " <> txt)

instance Db Program where
  readDb :: DbQuery -> Program (Either Text [DbRow])
  readDb queryStr = do
    logDebugN "readDb start"
    sqlitePath <- connStr <$> lift ask
    let q = fromString (unpack queryStr) :: Query
    liftIO $ catch (bracket (open sqlitePath) close
      (\conn -> do
        res <- query_ conn q :: IO [DbRow]
        return $ Right res)) (\e -> return $ Left (pack (show (e :: SomeException))))
        
  writeDb :: DbQuery -> Program (Either Text ())
  writeDb queryStr = do
    sqlitePath <- connStr <$> lift ask
    let q = fromString (unpack queryStr) :: Query
    liftIO $ catch (bracket (open sqlitePath) close
      (\conn -> Right <$> execute_ conn q)) (\(SomeException e) -> return $ Left (pack (displayException e)))
