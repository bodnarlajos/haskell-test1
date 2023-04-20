{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module TestImpl where

import Db ( Printer (print, printRows), Db (readDb, writeDb), DbQuery )
import Data.Text ( Text )
import DbEntities (DbRow (..))
import Control.Monad.Trans.Reader (ReaderT(..))
import RepositoryC (RepositoryC (getData), Data (Data), DataId)

newtype TestProgram a = TestProgram a

instance Functor TestProgram where
  fmap :: (a -> b) -> TestProgram a -> TestProgram b
  fmap fv (TestProgram a) = TestProgram (fv a)
instance Applicative TestProgram where
  pure :: a -> TestProgram a
  pure a = TestProgram a
  (<*>) :: TestProgram (a -> b) -> TestProgram a -> TestProgram b
  (TestProgram fa) <*> (TestProgram b) = TestProgram (fa b)
instance Monad TestProgram where
  return :: a -> TestProgram a
  return a = TestProgram a
  (>>=) :: TestProgram a -> (a -> TestProgram b) -> TestProgram b
  (TestProgram a) >>= fb = fb a

instance Db TestProgram where
  readDb :: Db m => DbQuery -> m (Either Text [DbRow])
  readDb _ = return $ Right [DbRow 1 "Test data"]
  writeDb :: DbQuery -> TestProgram (Either Text ())
  writeDb _ = return $ Right ()

instance Printer TestProgram where
  print :: Text -> TestProgram ()
  print _ = return ()
  printRows :: [DbRow] -> TestProgram ()
  printRows _ = return ()

instance RepositoryC TestProgram where
  getData :: RepositoryC.DataId -> TestProgram Data
  getData _ = return $ Data 1 "Test data"

runTestProgram :: TestProgram Text -> TestProgram Text
runTestProgram fv = fv
