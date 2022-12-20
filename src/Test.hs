{-# LANGUAGE InstanceSigs #-}

module Test where
import Data.Type.Bool (Not)

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

test3 :: TestMonad Char Char [Char]
test3 = TestMonad $ \a b -> Just $ a : [b]

test2 :: TestMonad Char Char [Char]
test2 = TestMonad $ \a b -> Just $ (a : [b]) ++ [a]

test :: Maybe [Char]
test = case runTestMonad test3 'a' 'b' of
  Nothing -> Nothing
  Just s -> case runTestMonad test2 'c' 'd' of
    Nothing -> Nothing
    Just s' -> Just $ s ++ s'
  
fvTestMonad :: IO ()
fvTestMonad = let res = test
                in print res