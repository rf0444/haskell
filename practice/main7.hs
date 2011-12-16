import Data.List (intercalate)
import Data.Char (toUpper)
import System.IO

main :: IO ()
main =
-- {{{ 7.1
 do
  putStr "> "
  inputStr <- getLine
  let ws = words inputStr
  putStrLn $ intercalate "," ws
-- }}}
---- {{{ 7.4
------ {{{ 7.4.1
---- do
----  putStr "> "
----  fileName <- getLine
----  file <- openFile fileName ReadMode
----  fileContents <- hGetContents file
----  let out = process fileContents
----  putStrLn out
----  hClose file
------ }}}
------ {{{ 7.4.2
---- do
----  putStr "> "
----  fileName <- getLine
----  fileContents <- readFile fileName
----  let out = process fileContents
----  putStrLn out
------ }}}
------ {{{ 7.4.4
---- interact process
------ }}}
-- where
--  process :: String -> String
--  process = map toUpper
---- }}}

