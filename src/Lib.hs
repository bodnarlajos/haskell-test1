{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Db
import Data.Text (Text, empty)
import DbEntities
import DbImpl (Program, runProgram)

someFunc :: IO ()
someFunc = do
  result <- runProgram start (DI "logfile.txt" "./db.sqlite") :: IO Text
  Prelude.print result

start :: Program Text
start = worker

worker :: (Db m, Printer m) => m Text
worker = do
  res <- readDb "select * from test"
  case res of
    Left err -> Db.print err
    Right res' -> printRows res'
  writeWasOkay <- writeDb "insert into test(text) values('new')"
  case writeWasOkay of
    Left err -> Db.print $ "There was an exception: " <> err
    Right _ -> return ()
  res' <- readDb "select * from test where id = 2"
  case res' of
    Left err -> Db.print err >> return empty
    Right value -> do
                   Db.printRows value
                   return $ getText $ head value
  where
    getText (DbRow _ txt) = txt
