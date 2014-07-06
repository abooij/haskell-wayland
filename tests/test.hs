import Graphics.Wayland

main = do
  a <- displayConnect
  print a
  b <- displaySync a
  print b
