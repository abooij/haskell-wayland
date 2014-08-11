import Control.Concurrent

import Graphics.Wayland.Client

main = do
  a <- displayConnect
  print a
  b <- displaySync a
  print b
  let listener = CallbackListener {
    callbackDone = \ _ _ -> putStrLn "received done"
    }
  callbackSetListener b listener
  displayFlush a
  displayGetFd a >>= threadWaitRead
  displayDispatch a
