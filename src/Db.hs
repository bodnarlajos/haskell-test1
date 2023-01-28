module Db where

import Data.Text (Text)
import DbEntities

type DbQuery = Text
newtype DbConfig = DbConfig { dbPath :: FilePath }

class (Monad m) => Db m where
  readDb :: DbQuery -> m [DbRow]
  writeDb :: DbQuery -> m ()
  getConfig :: m DbConfig

class (Monad m) => Printer m where
  print :: (Show a) => a -> m ()
