{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Db
import TestLogger
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)

someFunc :: IO ()
someFunc = do
  result <- runProgram worker (DI "logfile" "./db.sqlite")
  Prelude.print result

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
