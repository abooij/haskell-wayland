module Graphics.Wayland.Internal.Util (
  CMessage, CInterface, Client
  ) where

import Data.Functor
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <wayland-server.h>
#include <wayland-util.h>

{#context prefix="wl"#}


-- Data types
-- struct wl_message {
--         const char *name;
--         const char *signature;
--         const struct wl_interface **types;
-- };
-- | struct wl_message pointer
{#pointer * message as CMessage newtype#}
-- messageName msg = unsafePerformIO $ peekCString <$> {#get message->name#} msg
-- messageSignature msg = unsafePerformIO $ peekCString <$> {#get message->signature#} msg
-- messageInterface msg = unsafePerformIO

-- struct wl_interface {
--         const char *name;
--         int version;
--         int method_count;
--         const struct wl_message *methods;
--         int event_count;
--         const struct wl_message *events;
-- };
-- | struct wl_interface pointer
{#pointer * interface as CInterface newtype#}



-- opaque server-side wl_client struct
{#pointer * client as Client#}
