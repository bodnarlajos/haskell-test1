{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Db
import Data.Text (Text)
import DbEntities
import DbImpl (Program, runProgram)

someFunc :: IO ()
someFunc = do
  result <- runProgram start (DI "logfile.txt" "./db.sqlite") :: IO Text
  Prelude.print result

start :: Program Text
start = worker
