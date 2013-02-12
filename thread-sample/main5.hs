import Control.Concurrent
import Control.Exception
import Data.Function (fix)
import Data.IORef

main :: IO ()
main = do
    putStrLn "begin"
    ref <- newIORef 0
    x <- awaitUntil (> 10000) 1 $ do
        modifyIORef ref succ
        x <- readIORef ref
        putStrLn $ "x: " ++ show x
        return x
    putStrLn $ "end: " ++ show x

awaitUntil :: (a -> Bool) -> Int -> IO a -> IO a
awaitUntil p milliSec action = do
    future <- fork . fix $ \loop -> do
        x <- action
        if p x
            then
                return x
            else do
                threadDelay $ milliSec * 1000
                loop
    future

fork :: IO a -> IO (IO a)
fork proc = do
    mvar <- newEmptyMVar
    forkFinally proc $ putMVar mvar
    return $ takeMVar mvar >>= either throwIO return

-- copy from base-4.6
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
      forkIO $ try (restore action) >>= and_then
