module RepositoryC where

import Db
import Data.Text (Text)

type DataId = Int
data Data = Data DataId Text

class (Monad m, Db m) => RepositoryC m where
  getData :: DataId -> m Data