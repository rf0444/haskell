import Control.Concurrent

main :: IO ()
main = do
    putStrLn "begin"
    mvar <- newEmptyMVar
    forkIO $ thread mvar 1
    takeMVar mvar
    putStrLn "end"
  where
    thread mvar x = do
        putStrLn $ "x: " ++ show x
        if x > 10000 then putMVar mvar () else do
            threadDelay 1000
            thread mvar $ succ x
