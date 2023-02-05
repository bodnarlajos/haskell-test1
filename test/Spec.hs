{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
import Control.Exception (bracket, throw, SomeException (SomeException), Exception, catch, ArithException (DivideByZero))
import Lib (someFunc)
import Test.QuickCheck (quickCheck, Testable, Property)
import Test.QuickCheck.Monadic
data MyException = MyException | MyBeginException deriving Show
instance Exception MyException

-- main :: IO ()
-- main = do
--   putStrLn "test1"
--   res <- catch (bracket (throw MyBeginException >> return 1) (\i -> print $ "finally: " ++ show i) (throw MyException)) (\(SomeException e) -> return $ show e)
--   print res
--   print "test1 finish"

-- main :: IO ()
-- main = someFunc

-- main :: IO ()
-- main = do
--   res <- catch (throw Control.Exception.DivideByZero) (\e -> putStrLn (show (e :: SomeException)) >> return ())
--   return res

newtype TestProgi a = TestProgi a deriving (Show, Functor, Applicative, Monad)

test ::  Bool
test = True

main = do
  quickCheck test
  quickCheck monadTest

monadTest :: Property
monadTest = monadicIO $ do 
                          result <- run testIO
                          assert (result == "valame")

testIO :: IO String
testIO = do
  putStrLn "hello te lo testIO"
  return "valamie"