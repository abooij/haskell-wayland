import Control.Concurrent

import Graphics.Wayland.Client

main = do
  connect <- displayConnect
  print connect
  let display = case connect of
                  Just x -> x
                  Nothing -> error "could not connect to a wayland server"
  b <- displaySync display
  print b
  let listener = CallbackListener {
    callbackDone = \ _ _ -> putStrLn "received done"
    }
  callbackSetListener b listener
  displayFlush display
  displayGetFd display >>= threadWaitRead
  displayDispatch display
