{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module TestGADT where

data TestGadts a where
  GI :: Int -> TestGadts Int
  GS :: String -> TestGadts String

-- instance Functor TestGadts where
--   fmap :: (a -> b) -> TestGadts a -> TestGadts b
--   fmap fv (GS a) = GS (fv a)

instance Show (TestGadts a) where
  show :: TestGadts a -> String
  show (GI a) = show a
  show (GS a) = show a 

testGadts :: IO ()
testGadts = do
  let a = GI 1
  let b = GS "hello"
  print b
  print a
  return ()

testFv1 :: TestGadts Int -> IO ()
testFv1 input = do
  let a = show input
  return ()
