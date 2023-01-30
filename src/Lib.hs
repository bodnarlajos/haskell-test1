{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Lib
    ( someFunc
    ) where

import Db
import Data.Text (Text)
import DbEntities
import Control.Monad.IO.Class (liftIO)
import DbImpl (Program, runProgram)

someFunc :: IO ()
someFunc = do
  result <- runProgram start (DI "logfile" "./db.sqlite") :: IO Text
  Prelude.print result

start :: Program IO Text
start = liftIO worker
