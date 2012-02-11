import System.IO

main :: IO ()
main = do
  hGetBuffering stdout >>= putStrLn . show
  hSetBuffering stdout NoBuffering
  putStr "> "
  s <- getLine
  putStrLn s

