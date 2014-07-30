module Graphics.Wayland.Scanner.Names (
  ServerClient(..),

  requestInternalCName, eventInternalCName,
  requestForeignCName, eventForeignCName,
  requestHaskName, eventHaskName,

  interfaceTypeName,
  enumTypeName, enumEntryHaskName,
  messageListenerTypeName,
  messageListenerWrapperName,

  capitalize
  ) where

import Data.Char
import Data.List
import Language.Haskell.TH

import Graphics.Wayland.Scanner.Types



-- The functions below are a bit sad. The functions in the Wayland protocol header files are declared "static inline", meaning that they disappear after compilation.
-- Instead, we parse this file and remove exactly the static inline stuff, to get linkable variants of them. They have "x_" prepended.
requestInternalCName :: InterfaceName -> MessageName -> Name
requestInternalCName iface msg = mkName $ "x_" ++ iface ++ "_" ++ msg
eventInternalCName :: InterfaceName -> MessageName -> Name
eventInternalCName iface msg = mkName $ "x_" ++ iface ++ "_" ++ msg

requestForeignCName :: InterfaceName -> MessageName -> String
requestForeignCName iface msg = "x_" ++ iface ++ "_" ++ msg
eventForeignCName :: ServerClient -> InterfaceName -> MessageName -> String
eventForeignCName Server iface msg = "x_" ++ iface ++ "_send_" ++ msg
eventForeignCName Client iface msg = "x_" ++ iface ++ "_" ++ msg
-- Badness ends here

requestHaskName :: ProtocolName -> InterfaceName -> MessageName -> Name
requestHaskName pname iname mname = mkName $ toCamel (haskifyInterfaceName pname iname ++ "_" ++ mname)
eventHaskName :: ProtocolName -> InterfaceName -> MessageName -> Name
eventHaskName   pname iname mname = mkName $ toCamel (haskifyInterfaceName pname iname ++ "_" ++ mname)
enumEntryHaskName :: ProtocolName -> InterfaceName -> EnumName -> String -> Name
enumEntryHaskName pname iname ename entryName =
  mkName $ haskifyInterfaceName pname iname ++ capitalize (toCamel ename) ++ capitalize (toCamel entryName)

interfaceTypeName :: ProtocolName -> InterfaceName -> Name
interfaceTypeName pname iname = mkName $ capitalize $ haskifyInterfaceName pname iname

enumTypeName :: ProtocolName -> InterfaceName -> EnumName -> Name
enumTypeName pname iname ename = mkName $ capitalize $ haskifyInterfaceName pname iname ++ capitalize (toCamel ename)

messageListenerTypeName :: ServerClient -> ProtocolName -> InterfaceName -> Name
messageListenerTypeName Server pname iname = mkName $ capitalize (haskifyInterfaceName pname iname) ++ "Interface"
messageListenerTypeName Client pname iname = mkName $ capitalize (haskifyInterfaceName pname iname) ++ "Listener"

messageListenerWrapperName :: ServerClient -> InterfaceName -> MessageName -> Name
messageListenerWrapperName Client iname mname = mkName $ iname ++ "_" ++ mname ++ "_listener_wrapper"
messageListenerWrapperName Server iname mname = mkName $ iname ++ "_" ++ mname ++ "_interface_wrapper"

-- | Some interfaces use a naming convention where wl_ or their protocol's name is prepended.
--   We remove both because it doesn't look very Haskelly.
haskifyInterfaceName :: ProtocolName -> InterfaceName -> String
haskifyInterfaceName pname iname =
  toCamel $ removeInitial (pname ++ "_") $ removeInitial "wl_" iname


-- stupid utility functions follow

-- | if the second argument starts with the first argument, strip that start
removeInitial :: Eq a => [a] -> [a] -> [a]
removeInitial remove input = if remove `isPrefixOf` input
                                     then drop (length remove) input
                                     else input

-- | convert some_string to someString
toCamel :: String -> String
toCamel (a:'_':c:d) | isAlpha a, isAlpha c = a : toUpper c : toCamel d
toCamel (a:b) = a : toCamel b
toCamel x = x


capitalize :: String -> String
capitalize x = toUpper (head x) : tail x

-- decapitalize :: String -> String
-- decapitalize x = toLower (head x) : tail x
