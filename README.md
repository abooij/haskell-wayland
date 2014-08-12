# Haskell Wayland bindings #
Uh... these are what you'd expect.

> NOTE: obviously this thing is incomplete, and needs more documentation and stuff. I'm very happy to work on that, but please let me know what **you** think requires attention.

Refer to `NOTES.md` for more notes on wayland terminology, how it works, and ways to shoot yourself in the foot.

## API design and symbol naming ##

The majority of the Wayland API is based on an object-oriented event framework.
The objects have a type, which wayland calls an _interface_.
A _protocol_ defines a list of such interfaces.

Haskell renames these interfaces by, if possible, removing `wl_`, and then converting to CamelCase.
For example:

- `wl_display` is called `Display` in haskell-wayland
- `wl_registry` -> `Registry`
- `xdg_shell` -> `XdgShell` (as of this writing, however, `xdg_shell` is not in the default wayland protocol - but you can access it by generating the haskell-wayland API using the corresponding protocol XML files)
- ...

Wayland names the actions on these interfaces e.g. `wl_display_connect` or `wl_compositor_create_region`. haskell-wayland converts these names into camelCase, so that you would call `displayConnect` or `compositorCreateRegion`.


## Splicing a different protocol XML file ##

Wayland has a "core" API (specified by `wayland-{client,server,util,egl,cursor}.h`, bound in `Graphics.Wayland.Internal.{Client,Server,...}`), and on top of that generates two header files (`wayland-{client,server}-protocol.h`) from an XML file detailing the wayland wire protocol.
A wayland compositor might support several such protocols (e.g. as of this writing, weston supports the core `wayland.xml` protocol, as well as `desktop-shell.xml`, `fullscreen-shell.xml`, `input-method.xml`, `screenshooter.xml`, ...).

The program that generates these protocol header files is called a _scanner_, and wayland ships with `wayland-scanner`.
For haskell-wayland, you can find the equivalent in `Graphics.Wayland.Scanner`.
Its purpose is to bind to the C wayland interface and marshall all values.

To have haskell-wayland generate a haskell API to other such XML files (the `wayland.xml` is always generated), you'll want to copy what I did in `Graphics.Wayland.Internal.SpliceClient`, `Graphics.Wayland.Internal.SpliceClientInternal` and `Graphics.Wayland.Internal.SpliceClientTypes` (but this might change to fix what symbols that are exposed to the user).
Only the first two are to be exported to the user (see `Graphics.Wayland.Client` and notice that `Graphics.Wayland.Internal.SpliceClientInternal` is absent).


## Value marshalling ##

Wherever possible, all C values are marshalled to Haskell equivalents.
For the protocol API, this is done by `Graphics.Wayland.Scanner.Marshall`, and for the fixed api manually (but that's mostly trivial).

The exceptions to this are e.g. the methods that give you access to the memory contained by a buffer (which as of writing I haven't implemented yet).


## Technical notices ##

In theory, the symbols exposed by the C scanner (`wayland-scanner`) are off-limits for us: every language is supposed to only bind to the C library functions in `libwayland-client` and `libwayland-server`. In other words, the C library functions exposed in `wayland-{client,server,util,egl,cursor}.h`, plus the protocol XML files, should suffice to bind to all of wayland. However, in one occasion we do make us of them (binding a list of `struct wl_interface`s).

Since Template Haskell doesn't yet support splicing the module statement, or otherwise dynamically specifying which symbols get exported, currently you can access some `_request_binding` functions and `_c_interface` values, e.g. `wl_data_offer_c_add_listener_request_binding` and `wl_keyboard_c_interface`. These should be internal, and one of these days I'll fix that.

In terms of safety, there are plenty of opportunities with this library to shoot yourself in the foot.
For the most part, on the client side you'll want to stick to C-style event loops with appropriate polls: an example is (somewhat) provided.


## Debugging ##

Try using [wayland-tracker](https://github.com/01org/wayland-tracker) if your code won't work at all: it is a program that can dump the connection between a server and a client.


## TODO ##

- fix FIXMEs and TODOs in source
- be smart about argument names to find better marshalling types. e.g. a uint argument with name "fd" should be marshalled to a haskell Fd. a uint named "time" should be a Data.Time.Clock.POSIX.POSIXTime.
- prettify binding to wl_registry.bind (ie make more type-safe, add haskell documentation, etc)
- fix exposed symbols
- some kind of fancy FRP library binding?
- write documentation strings from .protocol files into haddock???
- allow easy building of other .protocol files into haskell bindings
- protocol version checker function
- check if returned objects may be NULL in fixed API (protocol-generated API should be fine since object construction is guaranteed to work and potentially NULL objects should be advertised as nullable in the XML files)
