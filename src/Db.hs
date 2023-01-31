{-# LANGUAGE OverloadedStrings #-}
module Db where

import Data.Text (Text)
import DbEntities

type DbQuery = Text
newtype DbConfig = DbConfig { dbPath :: FilePath }

class (Monad m) => Db m where
  readDb :: DbQuery -> m [DbRow]
  writeDb :: DbQuery -> m ()

class (Monad m) => Printer m where
  print :: (Show a) => a -> m ()

worker :: (Db m, Printer m) => m Text
worker = do
  res <- readDb "select * from test"
  Db.print res
  writeDb "insert into test(text) values('new')"
  res' <- readDb "select * from test"
  Db.print res'
  return $ getText $ head res'
  where
    getText (DbRow _ txt) = txt
