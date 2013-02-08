import Control.Concurrent

main :: IO ()
main = do
    putStrLn "begin"
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar
    forkIO $ do
        putStrLn "hello"
        threadDelay 1000000
        putStrLn "hello"
        threadDelay 1000000
        putStrLn "hello"
        putMVar mvar1 ()
    forkIO $ do
        threadDelay 500000
        putStrLn "world"
        threadDelay 1000000
        putStrLn "world"
        threadDelay 1000000
        putStrLn "world"
        putMVar mvar2 ()
    takeMVar mvar1
    takeMVar mvar2
    putStrLn "end"
