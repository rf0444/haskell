import Control.Monad.Reader
import System.Environment (getArgs)

main :: IO ()
main = (getConf >>=) . runReaderT $ do
    printFirstData
    printSecondData

printFirstData :: ReaderT Conf IO ()
printFirstData = do
    printData "A"

printSecondData :: ReaderT Conf IO ()
printSecondData = do
    printData "B"

printData :: String -> ReaderT Conf IO ()
printData kind = ReaderT $ do
    putStrLn . toPrintStr kind
  where
    toPrintStr "A" = confA
    toPrintStr "B" = confB
    toPrintStr _ = confA

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
