import Control.Exception (bracket, throw, SomeException (SomeException), Exception, catch)
import Lib (someFunc)
data MyException = MyException | MyBeginException deriving Show
instance Exception MyException

-- main :: IO ()
-- main = do
--   putStrLn "test1"
--   res <- catch (bracket (throw MyBeginException >> return 1) (\i -> print $ "finally: " ++ show i) (throw MyException)) (\(SomeException e) -> return $ show e)
--   print res
--   print "test1 finish"

main :: IO ()
main = someFunc