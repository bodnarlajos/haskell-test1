module DbEntities where
import Data.Text
import Database.SQLite.Simple

data DbRow = DbRow Int Text deriving (Show)

instance FromRow DbRow where
  fromRow = DbRow <$> field <*> field
