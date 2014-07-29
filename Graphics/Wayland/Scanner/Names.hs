module Graphics.Wayland.Scanner.Names where

import Data.Char
import Data.List
import Language.Haskell.TH

import Graphics.Wayland.Scanner.Protocol

-- | get the wayland-style name for some request message
getRequestCName :: InterfaceName -> String -> String
getRequestCName iface msg = "x_"++iface ++ "_" ++ msg

-- | get the wayland-style name for some event message method (ie server-side)
getEventCName :: InterfaceName -> String -> String
getEventCName iface msg = "x_"++iface++"_send_"++msg

getRequestHaskName :: InterfaceName -> String -> String
getRequestHaskName iface msg = (toCamel $ removeInitial "wl_" iface) ++ (capitalize $ toCamel msg)

getEventHaskName :: InterfaceName -> String -> String
getEventHaskName iface msg = (toCamel $ removeInitial "wl_" iface) ++ "Send" ++ (capitalize $ toCamel msg)

-- | takes a wayland-style message name and interface context and generates a pretty Haskell-style function name
messageHaskName :: String -> String
messageHaskName = toCamel . removeInitial "wl_"

-- | takes a wayland-style interface name and generates a TH name for types
interfaceTypeName :: InterfaceName -> Name
interfaceTypeName = mkName . prettyInterfaceName

-- | convert some_string to someString
toCamel :: String -> String
toCamel (a:'_':c:d) | isAlpha a, isAlpha c = a : (toUpper c) : (toCamel d)
toCamel (a:b) = a : toCamel b
toCamel x = x

-- | if the second argument starts with the first argument, strip that start
removeInitial :: Eq a => [a] -> [a] -> [a]
removeInitial remove input = if isPrefixOf remove input
                                     then drop (length remove) input
                                     else input

prettyInterfaceName :: String -> String
prettyInterfaceName = capitalize . toCamel . removeInitial "wl_"

prettyMessageName :: String -> String -> String
prettyMessageName ifacename msgname = toCamel $ ((removeInitial "wl_" ifacename) ++ "_" ++ msgname)

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b
