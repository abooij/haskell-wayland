{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Graphics.Wayland.Internal.SpliceServerInternal where

import Data.Functor
import Language.Haskell.TH
import Foreign.C.Types

import Graphics.Wayland.Scanner.Protocol
import Graphics.Wayland.Scanner
import Graphics.Wayland.Internal.SpliceServerTypes
import Graphics.Wayland.Internal.Util


$(runIO readProtocol >>= generateServerInternalMethods)
$(runIO readProtocol >>= generateServerInternalListeners)
