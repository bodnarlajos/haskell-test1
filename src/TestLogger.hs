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

data DI = DI { logFile :: FilePath, connStr :: String }
-- data Program m a = (Db m, Printer m, Monad m, Functor m, Applicative m) => Program { unProgram :: LoggingT (ReaderT DI m) a }
type Program m a = (Db m, Printer m, Monad m, Functor m, Applicative m) => LoggingT (ReaderT DI m) a


runProgram :: Program IO Text -> DI -> IO Text
runProgram worker di = runReaderT (runStdoutLoggingT worker) di

-- instance Db MP where
--   readDb :: DbQuery -> MP [Row]
--   readDb query'' = do
--     connStr' <- connStr <$> lift ask
--     conn <- liftIO $ open connStr'
--     let query' = Query query''
--     res <- liftIO $ query_ conn query' :: MP [DbRow]
--     liftIO $ close conn
--     return res
--   writeDb :: DbQuery -> MP ()
--   writeDb query'' = do
--     connStr' <- connStr <$> lift ask
--     conn <- liftIO $ open connStr'
--     let query' = Query query''
--     liftIO $ execute conn query' ()
--     liftIO $ close conn
  
-- instance Printer MP where
--   print a = liftIO $ Prelude.print a
