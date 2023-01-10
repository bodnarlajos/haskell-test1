{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module DbImpl where

import Db
import Database.SQLite.Simple (close, open, query_)
import Database.SQLite.Simple.FromRow (FromRow(..), FromRow)
import Database.SQLite.Simple (field)
import TestLogger

data DbImpl = DbImpl { dbPath :: FilePath }

instance DbData DbImpl where
  getConnStr = dbPath

data DbRow = DbRow Int String deriving (Show)

instance FromRow DbRow where
  fromRow = DbRow <$> field <*> field

instance DbOperation IO where
  readDb :: (DbData a) => a -> IO DbResult
  readDb db = do
    let sqlitePath = getConnStr db
    conn <- open sqlitePath
    res <- query_ conn "SELECT * FROM test" :: IO [DbRow]
    close conn 
    return $ DbResStrings $ map (\(DbRow i s) -> s) res
  writeDb _ = undefined

instance DbOperation MP where
  
  
