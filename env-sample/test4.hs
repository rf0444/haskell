import Control.Monad.Reader
import System.Environment (getArgs)
import Test.Hspec
import Test.Hspec.Core (SpecM)

main :: IO ()
main = (getConf >>=) . runReaderT $ do
    hspec `mapReaderT` testFirstData
    hspec `mapReaderT` testSecondData

testFirstData :: ReaderT Conf SpecM ()
testFirstData = do
    describe "print first data" `mapReaderT` do
        it "should not error" `mapReaderT` do
            printFirstData

testSecondData :: ReaderT Conf SpecM ()
testSecondData = do
    describe "print second data" `mapReaderT` do
        it "should not error" `mapReaderT` do
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
