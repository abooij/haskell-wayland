{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Graphics.Wayland.Internal.SpliceTypes where

import Data.Functor
import Language.Haskell.TH
import Foreign.C.Types

import Graphics.Wayland.Internal.Protocol
import Graphics.Wayland.Internal.Scanner

$(runIO $ generateTypes <$> readProtocol)
$(runIO $ generateEnums <$> readProtocol)
