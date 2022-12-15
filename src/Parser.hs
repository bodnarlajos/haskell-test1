{- https://serokell.io/blog/parser-combinators-in-haskell#implementing-a-simple-parser-combinator-from-scratch -}

{-# LANGUAGE LambdaCase #-}
module Parser where

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \case
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [Unexpected hd]

char :: Eq i => i -> Parser i e i
char i = satisfy (== i)