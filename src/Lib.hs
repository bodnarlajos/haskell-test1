{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( someFunc
    ) where

import Db
import Data.Text (Text)
import DbEntities
import TestLogger
import Control.Monad.IO.Class (liftIO)

someFunc :: IO ()
someFunc = do
  result <- runProgram start (DI "logfile" "./db.sqlite") :: IO Text
  Prelude.print result

start :: Program IO Text
start = liftIO worker

worker :: (Db m, Printer m) => m Text
worker = do
  res <- readDb "select * from test"
  Db.print res
  writeDb "insert into test(text) values('codebol')"
  res' <- readDb "select * from test"
  Db.print res'
  return $ getText $ head res'
  where
    getText (DbRow _ txt) = txt
