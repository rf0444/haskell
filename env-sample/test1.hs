import System.Environment (getArgs)
import Test.Hspec

main :: IO ()
main = do
    hspec testFirstData
    hspec testSecondData

testFirstData :: Spec
testFirstData = do
    describe "print first data" $ do
        it "should not error" $ do
            printFirstData

testSecondData :: Spec
testSecondData = do
    describe "print second data" $ do
        it "should not error" $ do
            printSecondData

printFirstData :: IO ()
printFirstData = do
    conf <- getConf
    printData conf "A"

printSecondData :: IO ()
printSecondData = do
    conf <- getConf
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
