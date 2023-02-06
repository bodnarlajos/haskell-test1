{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repository where

import RepositoryC
import DbImpl (Program)
import Db (Db(readDb), Printer (printRows, print))
import Data.String (IsString(fromString))
import DbEntities
import Data.Text (empty)

instance RepositoryC Program where
  getData :: DataId -> Program Data
  getData dataId = do
    r <- readDb $ fromString ("select * from test where id = " <> show dataId)
    case r of
      Left err -> do
                    Db.print $ "error: " <> err
                    return $ Data 0 empty
      Right r' -> let (DbRow rowId rowTxt) = head r'
                    in return $ Data rowId rowTxt
