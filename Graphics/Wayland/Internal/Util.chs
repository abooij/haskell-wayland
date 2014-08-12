{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.Wayland.Internal.Util (
  CInterface(..), Client(..), Fixed256, Precision256
  ) where

import Data.Fixed (Fixed(..), HasResolution(..))
import Data.Typeable
import Data.Functor
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <wayland-server.h>
#include <wayland-util.h>

{#context prefix="wl"#}


-- | struct wl_interface pointer
{#pointer * interface as CInterface newtype#}



-- | opaque server-side wl_client struct
newtype Client = Client (Ptr Client) deriving (Eq)

-- | 8 bits of precision means a resolution of 256.
data Precision256 = Precision256 deriving (Typeable)
instance HasResolution Precision256 where
  resolution _ = 256
-- | Fixed point number with 8 bits of decimal precision.
--
--   The equivalent of wayland's wl_fixed_t.
type Fixed256 = Fixed Precision256
