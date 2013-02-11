import Control.Concurrent
import Control.Exception (SomeException, mask, throw, try)
import Control.Monad (join)

main :: IO ()
main = do
    putStrLn "begin"
    mvar <- fork $ do
        putStrLn "hello"
        error "error!"
        putStrLn "world"
    putStrLn "waitin..."
    join $ takeMVar mvar
    putStrLn "end"

fork :: IO a -> IO (MVar (IO a))
fork proc = do
    mvar <- newEmptyMVar
    forkFinally proc $ putMVar mvar . either throw return
    return mvar

-- copy from base-4.6
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
      forkIO $ try (restore action) >>= and_then
