{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Graphics.Wayland.Internal.SpliceProtocol where

import Data.Functor
import Language.Haskell.TH

import Graphics.Wayland.Internal.Protocol
import Graphics.Wayland.Internal.Scanner
import Foreign.C.Types

#include <wayland-server.h>
#include <wayland-client.h>

$(runIO $ generateTypes <$> readProtocol)
$((runIO $ readProtocol) >>= generateClientMethods)
