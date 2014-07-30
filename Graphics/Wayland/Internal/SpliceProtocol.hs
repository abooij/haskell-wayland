{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Graphics.Wayland.Internal.SpliceProtocol where

import Data.Functor
import Language.Haskell.TH
import Foreign.C.Types

import Graphics.Wayland.Scanner.Protocol
import Graphics.Wayland.Scanner
import Graphics.Wayland.Internal.SpliceTypes


$(runIO readProtocol >>= generateClientInternalMethods)
$(runIO readProtocol >>= generateServerInternalMethods)
