{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Internal.Scanner where

import Data.Functor
import Data.Maybe
import Data.Char
import Data.List
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
import Text.XML.Light
import System.Process
import System.IO
import System.Posix.Types
import Language.Haskell.TH

import Graphics.Wayland.Internal.Protocol

#include <wayland-server.h>

{#context prefix="wl"#}


generateTypes :: ProtocolSpec -> [Dec]
generateTypes ps = map generateInterface (specInterfaces ps) where
  generateInterface iface =
    let qname = mkName $ prettyInterfaceName $ interfaceName iface
    in
      (NewtypeD [] qname [] (NormalC qname [(NotStrict,AppT (ConT ''Ptr) (ConT qname))]) [])

generateClientMethods :: ProtocolSpec -> Q [Dec]
generateClientMethods ps = sequence $ concat $ (map generateEvents (specInterfaces ps)) ++ (map generateRequests (specInterfaces ps)) where
  generateEvents iface = [] -- callbacks...

  generateRequests iface = concat $ map generateRequest $ interfaceMethods iface where
    generateRequest msg =
      let iname = interfaceName iface
          mname = messageName msg
      in [
        -- This is the ccal declaration
        forImpD cCall unsafe (getMessageCName iname mname) (mkName $ (messageHaskName iname mname) ++ "'") (genMessageType iface msg)

      ]

-- generateServerMethods :: ProtocolSpec -> [Dec]


genMessageType :: Interface -> Message -> TypeQ
genMessageType iface msg =
  let
    numNewIds = sum $ map isNewId $ messageArguments msg
    isNewId arg = case arg of
                    (_, NewIdArg _, _) -> 1
                    _                  -> 0
    fixedArgs = if numNewIds==1
                   then filter notNewIds $ messageArguments msg
                   else messageArguments msg
    notNewIds arg = case arg of
                      (_, NewIdArg _, _) -> False
                      _                  -> True
    returnType = if numNewIds==1
                    then argTypeToType $ snd3 $ head $ filter (not.notNewIds) $ messageArguments msg
                    else [t|IO ()|]

  in
    foldr1 (\sometype curtype -> [t|$sometype -> $curtype|]) $ argTypeToType (ObjectArg (interfaceName iface)) : (map (argTypeToType.snd3) fixedArgs) ++ [returnType]

argTypeToType :: ArgumentType -> TypeQ
argTypeToType IntArg = [t| {#type int32_t#} |]
argTypeToType UIntArg = [t| {#type uint32_t#} |]
argTypeToType FixedArg = [t|{#type fixed_t#}|]
argTypeToType StringArg = [t| Ptr CChar |]
argTypeToType (ObjectArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToType (NewIdArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToType ArrayArg = undefined
argTypeToType FdArg = [t| {#type int32_t#} |]

-- | get the wayland-style name for some message
getMessageCName :: InterfaceName -> String -> String
getMessageCName iface msg = "x_"++iface ++ "_" ++ msg

-- | takes a wayland-style message name and interface context and generates a pretty Haskell-style function name
messageHaskName :: InterfaceName -> String -> String
messageHaskName iface msg = toCamel $ removeInitial "wl_" $ getMessageCName iface msg

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
