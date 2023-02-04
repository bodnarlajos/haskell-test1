{-# LANGUAGE OverloadedStrings #-}
module Db where

import Data.Text (Text, empty)
import DbEntities

type DbQuery = Text
newtype DbConfig = DbConfig { dbPath :: FilePath }

class (Monad m) => Db m where
  readDb :: DbQuery -> m (Either Text [DbRow])
  writeDb :: DbQuery -> m (Either Text ())

class (Monad m) => Printer m where
  print :: Text -> m ()
  printRows :: [DbRow] -> m ()

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
    Left err -> Db.print err
    Right res'' -> printRows res''
  case res' of
    Left err -> Db.print err >> return empty
    Right value -> return $ getText $ head value
  where
    getText (DbRow _ txt) = txt
