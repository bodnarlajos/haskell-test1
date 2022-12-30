{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestLogger where

import Control.Monad.Logger
    ( logDebug, runStdoutLoggingT, LoggingT )
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Control.Monad.Trans.Reader

-- https://github.com/hasura/haskell-server-boilerplate/blob/master/src/Hasura/HTTP/Server.hs

newtype MyProgram m r a = MyProgram { runMyProgram :: LoggingT (ReaderT r m) a }
  deriving (Generic, Functor, Applicative, Monad)

-- runTestLogger :: (MonadIO m) => m () -> m ()
-- runTestLogger method = do
--   result <- runStdoutLoggingT (run method)
--   return ()
--   where 
--     run :: ((T.Text -> IO ()) -> IO ()) -> LoggingT IO String
--     run method = do
--       l <- liftIO getLine
--       _ <- liftIO $ method $(logDebug)
--       $(logDebug) $ T.pack ("test it" ++ l)
--       return "valami"

