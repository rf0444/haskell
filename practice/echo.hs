import System.IO
import Control.Applicative

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  proc (/= "exit")
  putStrLn "exit program."
 where
  proc :: (String -> Bool) -> IO [String]
  proc f = takeWhileM f procs
   where
    procs :: [IO String]
    procs = (=<<) (printIf f) <$> prompt "> " <$> inputs

takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = return []
takeWhileM f (x:xs) = do
  ele <- x
  if f ele then do
    xs' <- takeWhileM f xs
    return (ele : xs')
  else return []

inputs :: [IO String]
inputs = repeat getLine

prompt :: String -> IO String -> IO String
prompt = (>>) . putStr

printIf :: (String -> Bool) -> String -> IO String
printIf f ele
 | f ele = putStrLn ele >> return ele
 | otherwise = return ele

