import Control.Monad
import Control.Concurrent
import System.IO

import Graphics.Wayland.Client

main = do
  a <- displayConnect
  fd <- displayGetFd a
  putStrLn $ "Using file descriptor " ++ show fd
  putStrLn $ "Display at " ++ show a
  b <- displayGetRegistry a
  putStrLn $ "Registry at "++ show b
  let listener = RegistryListener {
    registryGlobal = \reg name ifacename version -> putStrLn $ "Received global " ++ show name ++ " (" ++ ifacename ++ ") version " ++ show version,
    registryGlobalRemove = \ _ _ -> return ()
    }
  errorCode <- registrySetListener b listener
  putStrLn $ "Setting registry listener... " ++ show errorCode

  res <- displayPrepareRead a
  putStrLn $ "Preparing read... " ++ show res
  flushed <- displayFlush a
  putStrLn $ "Flushed " ++ show flushed
  putStrLn "polling"
  threadWaitRead fd
  putStrLn $ "Ready to read."
  events <- displayReadEvents a
  putStrLn $ "Read display events: " ++ show events
  dispatched <- displayDispatchPending a
  putStrLn $ "Dispatched events: " ++ show dispatched
  displayDisconnect a
