import Control.Concurrent

main :: IO ()
main = do
    putStrLn "begin"
    mvar <- newEmptyMVar
    forkIO $ do
        putStrLn "hello"
        threadDelay 1000000
        putStrLn "hello"
        threadDelay 1000000
        putStrLn "hello"
        putMVar mvar ()
    takeMVar mvar
    putStrLn "end"
