module Graphics.Wayland (
  version,
  module Graphics.Wayland.Internal.Client,  -- this will need to be removed when the lib is at a more complete state
  module Graphics.Wayland.Internal.SpliceProtocol
   ) where

import Graphics.Wayland.Internal.Client
import Graphics.Wayland.Internal.Version
import Graphics.Wayland.Internal.SpliceProtocol
