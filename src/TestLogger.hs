{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module TestLogger(runProgram, MP(), DI(..)) where

import Control.Monad.Logger
    ( runStdoutLoggingT, LoggingT )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Db
import Control.Monad.Trans.Class (lift)
import Database.SQLite.Simple

data DI = DI { logFile :: FilePath, connStr :: String }

type MP = LoggingT (ReaderT DI IO)

runProgram :: MP a ->  DI -> IO a
runProgram worker di = liftIO $ runReaderT (runStdoutLoggingT worker) di

instance Db MP where
  readDb :: DbQuery -> MP [DbRow]
  readDb query'' = do
    connStr' <- connStr <$> lift ask
    conn <- liftIO $ open connStr'
    let query' = Query query''
    res <- liftIO $ query_ conn query' :: MP [DbRow]
    liftIO $ close conn
    return res
  writeDb query'' = do
    connStr' <- connStr <$> lift ask
    conn <- liftIO $ open connStr'
    let query' = Query query''
    liftIO $ execute conn query' ()
    liftIO $ close conn
  
instance FromRow DbRow where
  fromRow = DbRow <$> field <*> field

instance Printer MP where
  print a = liftIO $ Prelude.print a
