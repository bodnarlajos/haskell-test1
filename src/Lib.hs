{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Db
import Data.Text (Text, empty, pack)
import DbEntities
import DbImpl (Program, runProgram, ProgramC)
import RepositoryC
import TestImpl
import Repository
import Reflex

someFunc :: IO ()
someFunc = do
  result <- runProgram start (DI "logfile.txt" "./db.sqlite") :: IO Text
  Prelude.print result

startTest :: TestProgram Text
startTest = do
  res <- runTestProgram worker
  res' <- workerRepo
  return res

start :: Program Text
start = do
  _ <- worker
  workerRepo

workerRepo :: (ProgramC m) => m Text
workerRepo = do
  (Data dataId dataText) <- getData 12
  Db.print $ pack ("workerRepo: " <> show dataId <> ", ") <> dataText
  return dataText

worker2Test :: IO ()
worker2Test = do
  _ <- runProgram worker2 (DI "logfile.txt" "./db.sqlite")
  return ()

worker2 :: (ProgramC m) => m ()
worker2 = do
  Db.print "hello"
  _ <- Db.writeDb "insert into test(text) values('worker2')"
  return ()

worker :: (ProgramC m) => m Text
worker = do
  (Data _ textFromData) <- getData 1
  Db.print textFromData
  res <- readDb "select * from test"
  case res of
    Left err -> Db.print err
    Right res' -> printRows res'
  writeWasOkay <- writeDb "insert into test(text) values('new')"
  case writeWasOkay of
    Left err -> Db.print $ "There was an exception: " <> err
    Right _ -> return ()
  _ <- workerRepo
  res' <- readDb "select * from test where id = 2"
  case res' of
    Left err -> Db.print err >> return empty
    Right value -> do
                   printRows value
                   return $ getText $ head value
  where
    getText (DbRow _ txt) = txt

-- startReflex = mainWidget $ el "div" $ text "Welcome to Reflex"
