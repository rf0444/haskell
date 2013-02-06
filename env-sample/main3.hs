import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
    printFirstData
    printSecondData

printFirstData :: IO ()
printFirstData = do
    printData "A"

printSecondData :: IO ()
printSecondData = do
    printData "B"

printData :: String -> IO ()
printData kind = do
    putStrLn $ toPrintStr kind
  where
    toPrintStr "A" = confA conf
    toPrintStr "B" = confB conf
    toPrintStr _ = confA conf

data Conf = Conf
    { confA :: String
    , confB :: String
    } deriving Show

getConf :: IO Conf
getConf = do
   putStrLn "getConf"
   args <- getArgs
   return . toConf $ args ++ ["aa", "bb"]
 where
   toConf (a:b:_) = Conf a b

conf :: Conf
conf = unsafePerformIO getConf
