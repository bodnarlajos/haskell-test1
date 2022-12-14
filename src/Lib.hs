module Lib
    ( someFunc
    ) where

import Db ( Db(Db) )

someFunc :: IO ()
someFunc = do
    let (Db str) = getFromDb2 <> getFromDb
    putStrLn str

getFromDb :: Db String
getFromDb = let a = "Test"
                b = "bakker"
                in ((<>b<>a) <$> getText) <> getText2

getFromDb2 :: Db String
getFromDb2 = do
    gt1 <- getText
    gt2 <- getText2
    return  $ gt1 <> gt2

getText :: Db String
getText = return "meg valami"

getText2 :: Db String
getText2 = return " no meg valami"
