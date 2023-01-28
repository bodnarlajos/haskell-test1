{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module DbImpl where

import Db
import DbEntities
import Database.SQLite.Simple
import Data.Text (unpack)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (ap)
import Control.Applicative (liftA2)

-- instance Applicative (Program IO) where
--   pure = return
--   (<*>) :: Program IO (a -> b) -> Program IO a -> Program IO b
--   (<*>) = liftA2

-- instance Monad (Program IO) where
--   return :: a -> Program IO a
--   return a = Program $ return a
--   (>>=) :: Program IO a -> (a -> Program IO b) -> Program IO b
--   a >>= fa = do
--     a' <- a
--     fa a'

instance Printer IO where
  print = Prelude.print

instance Db IO where
  readDb :: DbQuery -> IO [DbRow]
  readDb queryStr = do
    sqlitePath <- getConfig
    let sqlitePath' = dbPath sqlitePath
    conn <- liftIO $ open sqlitePath'
    let q = read (unpack queryStr) :: Query
    res <- liftIO $ query_ conn q :: IO [DbRow]
    liftIO $ close conn 
    return res
  writeDb _ = undefined
  getConfig = return $ DbConfig "sqlite.db"
  
  
