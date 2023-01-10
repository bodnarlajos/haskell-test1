module Db (Db(..), DbQuery, DbRow(..), Printer(..)) where

import Data.Text (Text)

type DbQuery = Text
data DbRow = DbRow Int Text deriving (Show)

class (Monad m) => Db m where
  readDb :: DbQuery -> m [DbRow]
  writeDb :: DbQuery -> m ()

class (Monad m) => Printer m where
  print :: (Show a) => a -> m ()
