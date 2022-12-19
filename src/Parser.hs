{- https://serokell.io/blog/parser-combinators-in-haskell#implementing-a-simple-parser-combinator-from-scratch -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Parser where
import GHC.IO.Handle (NewlineMode(inputNL))

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }

instance Functor (Parser i e) where
  fmap :: (a -> b) -> Parser i e a -> Parser i e b
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) -> Right (f output, rest)

instance Applicative (Parser i e) where
  pure :: a -> Parser i e a
  pure a = Parser $ \input -> Right (a, input)
  (<*>) :: Parser i e (a -> b) -> Parser i e a -> Parser i e b
  (Parser f) <*> (Parser a) = Parser $ (\input -> case f input of
    Left e -> Left e
    Right (fv, r) -> case a r of
      Left ers -> Left ers
      Right (a', i') -> Right (fv a', i'))

instance Monad (Parser i e) where
  return :: a -> Parser i e a
  return = pure

  (>>=) :: Parser i e a -> (a -> Parser i e b) -> Parser i e b
  (Parser a) >>= k = Parser $ \input -> do
    (ret, sret) <- a input
    runParser (k ret) sret

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \case
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [Unexpected hd]

char :: (Eq i) => i -> Parser i e i
char i = satisfy (== i)

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

string2 :: Eq i => [i] -> Parser i e [i]
string2 [] = pure []
string2 (x : xs) = (:) <$> char x <*> string2 xs

t = (:) <$> char 'h'

test :: IO ()
test = do
  let result = case rp 'h' "hello" of
                Right (_, s) -> case rp 'e' s of
                  Right a' -> return a'
                  Left e' ->  Left e'
                Left e -> Left e
  print result
  let result2 = runParser (string "hello") "hello te lo" :: Either [Error Char String] ([Char], [Char])
  print result2
  where
    rp c str = runParser (char c) str :: Either [Error Char String] (Char, [Char])