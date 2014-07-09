{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Internal.Scanner where

import Data.Functor
import Control.Monad (liftM)
import Data.Maybe
import Data.Char
import Data.List
import Data.Word
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
      (NewtypeD [] qname [] (NormalC qname [(NotStrict,AppT (ConT ''Ptr) (ConT qname))]) [mkName "Show"])

makeEnumHaskName :: Interface -> WLEnum -> String
makeEnumHaskName iface wlenum = (prettyInterfaceName $ interfaceName iface) ++ (prettyInterfaceName $ enumName wlenum)

generateEnums :: ProtocolSpec -> [Dec]
generateEnums ps = concat $ map eachGenerateEnums (specInterfaces ps) where
  eachGenerateEnums :: Interface -> [Dec]
  eachGenerateEnums iface = concat $ map generateEnum $ interfaceEnums iface where
    generateEnum :: WLEnum -> [Dec]
    generateEnum wlenum =
      let qname = mkName $ makeEnumHaskName iface wlenum
      in
        NewtypeD [] qname [] (NormalC qname [(NotStrict, (ConT ''Int))]) [mkName "Show", mkName "Eq"]
        :
        map (\(entry, val) -> (ValD (VarP $ mkName $ decapitalize $ makeEnumHaskName iface wlenum ++ prettyInterfaceName entry) (NormalB $ AppE (ConE qname) $ LitE $ IntegerL $ toInteger val) [])) (enumEntries wlenum)

data ServerClient = Server | Client  deriving (Eq)
-- | generate FFI for a certain side of the API
generateInternalMethods :: ProtocolSpec -> ServerClient -> Q [Dec]
generateInternalMethods ps sc = liftM concat $ sequence $ map generateInterface $ filter (\iface -> if sc == Server then interfaceName iface /= "wl_display" else True) $ specInterfaces ps where
  generateInterface :: Interface -> Q [Dec]
  generateInterface iface = sequence $ if sc == Server
                                          then methodBindings
                                          else interfaceBinding : methodBindings   where
   -- Generate bindings to the wl_interface * constants (for proxy usage).
   interfaceBinding = (forImpD cCall unsafe ("& " ++ interfaceName iface ++ "_interface") (mkName $ interfaceName iface ++ "_interface") [t|Ptr Interface|]) -- the type here doesn't really make sense (since newtype Interface = Interface (Ptr Interface)), but whatever - just passing values around
   -- Generate bindings to requests
   methodBindings = map generateRequest $ if sc == Server then interfaceEvents iface else interfaceMethods iface where
    generateRequest :: Message -> Q Dec
    generateRequest msg =
      let iname = interfaceName iface
          mname = messageName msg
          cname = if sc == Server
                     then getEventCName iname mname
                     else getRequestCName iname mname
          hname = if sc == Server
                     then mkName $ messageHaskName $ getEventCName iname mname
                     else mkName $ messageHaskName $ getRequestCName iname mname
          pname = if sc == Server
                     then mkName $ getEventHaskName iname mname
                     else mkName $ getRequestHaskName iname mname
      in forImpD cCall unsafe cname hname (genMessageType msg)

generateExternalMethods :: ProtocolSpec -> ServerClient -> Q [Dec]
generateExternalMethods ps sc = liftM concat $ sequence $ map generateInterface $ filter (\iface -> if sc == Server then interfaceName iface /= "wl_display" else True) $ specInterfaces ps where
  generateInterface :: Interface -> Q [Dec]
  generateInterface iface = liftM concat $ sequence $ map generateRequest $ if sc == Server then interfaceEvents iface else interfaceMethods iface where
    generateRequest :: Message -> Q [Dec]
    generateRequest msg =
      let iname = interfaceName iface
          mname = messageName msg
          cname = if sc == Server
                     then getEventCName iname mname
                     else getRequestCName iname mname
          hname = if sc == Server
                     then mkName $ messageHaskName $ getEventCName iname mname
                     else mkName $ messageHaskName $ getRequestCName iname mname
          pname = if sc == Server
                     then mkName $ getEventHaskName iname mname
                     else mkName $ getRequestHaskName iname mname
      in do
         let funexp = return $ VarE hname
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
                 else [t|()|]
         -- gens <-  [d|$(return $ VarP pname) =  $(argTypeMarshaller (map snd3 $ fixedArgs) funexp) |]
         let (pats, fun) = argTypeMarshaller fixedArgs funexp
         gens <- [d|$(return $ VarP pname) = $(LamE pats <$> fun) |]
         return gens


generateClientInternalMethods :: ProtocolSpec -> Q [Dec]
generateClientInternalMethods ps = generateInternalMethods ps Client

generateServerInternalMethods :: ProtocolSpec -> Q [Dec]
generateServerInternalMethods ps = generateInternalMethods ps Server

generateClientExternalMethods :: ProtocolSpec -> Q [Dec]
generateClientExternalMethods ps = generateExternalMethods ps Client

generateServerExternalMethods :: ProtocolSpec -> Q [Dec]
generateServerExternalMethods ps = generateExternalMethods ps Server

genMessageType :: Message -> TypeQ
genMessageType msg =
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
                    else [t|()|]

  in
    foldr (\addtype curtype -> [t|$addtype -> $curtype|]) [t|IO $(returnType)|] $ (map (argTypeToType.snd3) fixedArgs)

argTypeToType :: ArgumentType -> TypeQ
argTypeToType IntArg = [t| {#type int32_t#} |]
argTypeToType UIntArg = [t| {#type uint32_t#} |]
argTypeToType FixedArg = [t|{#type fixed_t#}|]
argTypeToType StringArg = [t| Ptr CChar |]
argTypeToType (ObjectArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToType (NewIdArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToType ArrayArg = undefined
argTypeToType FdArg = [t| {#type int32_t#} |]

marshallerVar :: Argument -> Name
marshallerVar (name, _, _) = mkName name

argTypeMarshaller :: [Argument] -> ExpQ -> ([Pat], ExpQ)
argTypeMarshaller args fun =
  let vars = map marshallerVar args
      mk = return . VarE . marshallerVar
      applyMarshaller :: [Argument] -> ExpQ -> ExpQ
      applyMarshaller (arg@(_, IntArg, _):as) fun = [|$(applyMarshaller as [|$fun (fromIntegral ($(mk arg) :: Int) )|])|]
      applyMarshaller (arg@(_, UIntArg, _):as) fun = [|$(applyMarshaller as [|$fun (fromIntegral ($(mk arg) :: Word))|]) |]
      applyMarshaller (arg@(_, FixedArg, _):as) fun = [|$(applyMarshaller as [|$fun (fromIntegral ($(mk arg) :: Int))|]) |] -- FIXME double conversion stuff!
      applyMarshaller (arg@(_, StringArg, _):as) fun = [|withCString $(mk arg) (\cstr -> $(applyMarshaller as [|$fun cstr|]))|]
      applyMarshaller (arg@(_, (ObjectArg iname), _):as) fun = [|$(applyMarshaller as [|$fun $(mk arg)|]) |] -- FIXME Maybe
      applyMarshaller (arg@(_, (NewIdArg iname), _):as) fun = [|$(applyMarshaller as [|$fun $(mk arg) |])|] -- FIXME Maybe
      applyMarshaller (arg@(_, ArrayArg, _):as) fun = undefined
      applyMarshaller (arg@(_, FdArg, _):as) fun = [|$(applyMarshaller as [|$fun (unFd ($(mk arg)))|]) |]
      applyMarshaller [] fun = fun
  in  (map VarP vars, applyMarshaller args fun)

unFd (Fd k) = k

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
