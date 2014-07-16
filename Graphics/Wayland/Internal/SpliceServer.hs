{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Graphics.Wayland.Internal.SpliceServer where

import Data.Functor
import Language.Haskell.TH
import Foreign.C.Types

import Graphics.Wayland.Internal.Protocol
import Graphics.Wayland.Internal.Scanner
import Graphics.Wayland.Internal.SpliceProtocol
import Graphics.Wayland.Internal.SpliceTypes


$((runIO $ readProtocol) >>= generateServerExternalMethods)
$((runIO $ readProtocol) >>= generateServerListenersExternal)
