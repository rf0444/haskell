import Control.Concurrent
import Control.Exception (SomeException, mask, throwIO, try)

main :: IO ()
main = do
    putStrLn "begin"
    future <- fork $ do
        putStrLn "hello"
        error "error!"
        putStrLn "world"
    putStrLn "waiting..."
    _ <- future
    putStrLn "end"

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
