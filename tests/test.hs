import Graphics.Wayland.Client

main = do
  a <- displayConnect
  print a
  b <- displaySync a
  print b
