module Graphics.Wayland.Internal.Client (
  Display, Proxy, EventQueue,

  eventQueueDestroy,

  displayConnect, displayConnectName, displayConnectFd
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Posix.Types

#include <wayland-client.h>

{#context prefix="wl"#}


-- Data types

-- In the case of the Client side, these are all just abstract pointer objects.

-- | struct wl_display pointer
{#pointer * display as Display newtype#}

-- | struct wl_proxy pointer
{#pointer * proxy as Proxy newtype#}

-- | struct wl_event_queue pointer
{#pointer * event_queue as EventQueue newtype#}


-- Protocol

-- TODO


-- Functions/methods


-- void wl_event_queue_destroy(struct wl_event_queue *queue);
{#fun unsafe event_queue_destroy as ^ {`EventQueue'} -> `()'#}

-- void wl_proxy_marshal(struct wl_proxy *p, uint32_t opcode, ...);

-- void wl_proxy_marshal_array(struct wl_proxy *p, uint32_t opcode,
--                               union wl_argument *args);

-- struct wl_proxy *wl_proxy_create(struct wl_proxy *factory,
--                                    const struct wl_interface *interface);
-- struct wl_proxy *wl_proxy_marshal_constructor(struct wl_proxy *proxy,
--                                               uint32_t opcode,
--                                               const struct wl_interface *interface,
--                                               ...);
-- struct wl_proxy *
-- wl_proxy_marshal_array_constructor(struct wl_proxy *proxy,
--                                    uint32_t opcode, union wl_argument *args,
--                                    const struct wl_interface *interface);

-- void wl_proxy_destroy(struct wl_proxy *proxy);
-- int wl_proxy_add_listener(struct wl_proxy *proxy,
--                           void (**implementation)(void), void *data);
-- const void *wl_proxy_get_listener(struct wl_proxy *proxy);
-- int wl_proxy_add_dispatcher(struct wl_proxy *proxy,
--                             wl_dispatcher_func_t dispatcher_func,
--                             const void * dispatcher_data, void *data);
-- void wl_proxy_set_user_data(struct wl_proxy *proxy, void *user_data);
-- void *wl_proxy_get_user_data(struct wl_proxy *proxy);
-- uint32_t wl_proxy_get_id(struct wl_proxy *proxy);
-- const char *wl_proxy_get_class(struct wl_proxy *proxy);
-- void wl_proxy_set_queue(struct wl_proxy *proxy, struct wl_event_queue *queue);


-- struct wl_display *wl_display_connect(const char *name);
-- | Connect to a display with a specified name
{#fun unsafe display_connect as displayConnectName {`String'} -> `Display' #}

-- | Connect to the default display by passing a null pointer
displayConnect = {#call unsafe display_connect#} nullPtr

-- struct wl_display *wl_display_connect_to_fd(int fd);
unFd :: Fd -> CInt
unFd (Fd k) = k
-- | Connect to a display by file descriptor
{#fun unsafe display_connect_to_fd as displayConnectFd {unFd `Fd'} -> `Display' #}

-- void wl_display_disconnect(struct wl_display *display);
-- int wl_display_get_fd(struct wl_display *display);
-- int wl_display_dispatch(struct wl_display *display);
-- int wl_display_dispatch_queue(struct wl_display *display,
--                               struct wl_event_queue *queue);
-- int wl_display_dispatch_queue_pending(struct wl_display *display,
--                                       struct wl_event_queue *queue);
-- int wl_display_dispatch_pending(struct wl_display *display);
-- int wl_display_get_error(struct wl_display *display);

-- int wl_display_flush(struct wl_display *display);
-- int wl_display_roundtrip(struct wl_display *display);
-- struct wl_event_queue *wl_display_create_queue(struct wl_display *display);

-- int wl_display_prepare_read_queue(struct wl_display *display,
--                                   struct wl_event_queue *queue);
-- int wl_display_prepare_read(struct wl_display *display);
-- void wl_display_cancel_read(struct wl_display *display);
-- int wl_display_read_events(struct wl_display *display);

-- void wl_log_set_handler_client(wl_log_func_t handler);
