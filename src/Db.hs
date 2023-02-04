{-# LANGUAGE OverloadedStrings #-}
module Db where

import Data.Text (Text, empty)
import DbEntities

type DbQuery = Text
newtype DbConfig = DbConfig { dbPath :: FilePath }

class (Monad m) => Db m where
  readDb :: DbQuery -> m (Either Text [DbRow])
  writeDb :: DbQuery -> m (Either Text ())

class (Monad m) => Printer m where
  print :: Text -> m ()
  printRows :: [DbRow] -> m ()

