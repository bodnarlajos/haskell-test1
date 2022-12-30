{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test(fvTestMonad, test4) where
import Debug.Trace (trace)

newtype TestMonad i e a = TestMonad { runTestMonad :: i -> e -> Maybe a }

instance Functor (TestMonad i e) where
  fmap :: (a -> b) -> TestMonad i e a -> TestMonad i e b
  fmap fv (TestMonad a) = TestMonad (\i e -> case a i e of
    Nothing -> Nothing
    Just a' -> Just $ fv a')

instance Applicative (TestMonad i e) where
  pure :: a -> TestMonad i e a
  pure a = TestMonad (\_ _ -> Just a)
  (<*>) :: TestMonad i e (a -> b) -> TestMonad i e a -> TestMonad i e b
  (TestMonad fv) <*> (TestMonad b) = TestMonad $ \i e -> case b i e of
    Nothing -> Nothing
    Just a' -> case fv i e of
                Nothing -> Nothing
                Just fv' -> Just $ fv' a'

test3 :: Char -> TestMonad Char Char [Char]
test3 c = TestMonad $ \a b -> Just $ a : c : [b]

testStr :: TestMonad Char Char [String]
testStr = TestMonad $ \a b -> Just [a : [b]]

test2 :: TestMonad Char Char [Char]
test2 = TestMonad $ \a b -> Just $ (a : [b]) ++ [a]

test :: Maybe [Char]
test = case runTestMonad (test3 'c') 'a' 'b' of
  Nothing -> Nothing
  Just s -> case runTestMonad test2 'c' 'd' of
    Nothing -> Nothing
    Just s' -> Just $ trace ("\n1: " ++ s) s ++ trace ("\n2: " ++ s') s'

fvTestMonad :: IO ()
fvTestMonad = let res = test
                in print res

test4 :: [Maybe [Char]]
test4 = let rr1 = foldr (\a res -> let r = runTestMonad a 'a' 'b'
                            in res ++ [r]) [] [test3 'g', test2]
            rr2 = runTestMonad testStr 'a' 'b'
            in case rr1 of
              [] -> [Nothing]
              [Nothing] -> [Nothing]
              [Just rr1'] -> case rr2 of
                Nothing -> [Nothing]
                Just rr2' -> Just <$> (rr1': rr2')
              _ -> [Nothing]