{-# LANGUAGE InstanceSigs #-}

module Test where
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

test2 :: TestMonad Char Char [Char]
test2 = TestMonad $ \a b -> Just $ (a : [b]) ++ [a]

test :: Maybe [Char]
test = case runTestMonad (test3 'c') 'a' 'b' of
  Nothing -> Nothing
  Just s -> case runTestMonad test2 'c' 'd' of
    Nothing -> Nothing
    Just s' -> Just $ (trace ("\n1: " ++ s) s) ++ (trace ("\n2: " ++ s') s')
  
fvTestMonad :: IO ()
fvTestMonad = let res = test
                in print res

-- testSeq :: Eq i => [Char] -> TestMonad i e [Char]
-- testSeq [] = pure []
-- testSeq (x : xs) = _ <$> test2
