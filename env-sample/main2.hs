import System.Environment (getArgs)

main :: IO ()
main = do
    conf <- getConf
    printFirstData conf
    printSecondData conf

printFirstData :: Conf -> IO ()
printFirstData conf = do
    printData conf "A"

printSecondData :: Conf -> IO ()
printSecondData conf = do
    printData conf "B"

printData :: Conf -> String -> IO ()
printData conf kind = do
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
