import Graphics.Wayland

main = do
  a <- displayConnect
  print a
  b <- xWlDisplaySync a
  print b
