module Graphics.Wayland (
  version, Fixed256, Precision256, Time, Result(..), errToResult,
  diffTimeToTime, timeToDiffTime, scannedVersionOf
  ) where

import Foreign.C.Types

import Graphics.Wayland.Internal.Util
import Graphics.Wayland.Internal.Version


data Result = Success | Failure deriving (Eq, Show)
errToResult :: CInt -> Result
errToResult 0    = Success
errToResult (-1) = Failure

scannedVersionOf :: (ProtocolVersion a) => a -> Int
scannedVersionOf x = protocolVersion (Just x)
