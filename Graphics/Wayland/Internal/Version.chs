module Graphics.Wayland.Internal.Version (version, ProtocolVersion(..)) where

#include <wayland-version.h>

{#enum define VersionInt {WAYLAND_VERSION_MAJOR as MajorInt, WAYLAND_VERSION_MINOR as MinorInt, WAYLAND_VERSION_MICRO as MicroInt} deriving (Eq, Ord) #}

version = (fromEnum MajorInt, fromEnum MinorInt, fromEnum MicroInt)

class ProtocolVersion a where
  protocolVersion :: p a -> Int
