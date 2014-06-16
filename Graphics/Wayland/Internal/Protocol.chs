{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Internal.Protocol where

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
import Language.Haskell.TH

#include <wayland-server.h>
#include <wayland-util.h>

{#context prefix="wl"#}

--         const char *name;
--         int version;
--         int method_count;
--         const struct wl_message *methods;
--         int event_count;
--         const struct wl_message *events;
data Interface = Interface {
  interfaceName :: String,
  interfaceVersion :: Int,
  interfaceMethods :: [Message], -- ^ aka requests
  interfaceEvents :: [Message],
  interfaceEnums :: [WLEnum]
  } deriving (Show)

-- wayland style enum specification (not Prelude)
data WLEnum = WLEnum {
  enumName :: String,
  enumEntries :: [(String,Int)]
  } deriving (Show)
-- union wl_argument {
--         int32_t i; /**< signed integer */
--         uint32_t u; /**< unsigned integer */
--         wl_fixed_t f; /**< fixed point */
--         const char *s; /**< string */
--         struct wl_object *o; /**< object */
--         uint32_t n; /**< new_id */
--         struct wl_array *a; /**< array */
--         int32_t h; /**< file descriptor */
-- };

type InterfaceName = String
data ArgumentType = IntArg | UIntArg | FixedArg | StringArg | ObjectArg InterfaceName | NewIdArg InterfaceName | ArrayArg | FdArg deriving (Show)
argConversionTable :: [(String, ArgumentType)] -- for all easy argument types
argConversionTable = [
  ("int", IntArg),
  ("uint", UIntArg),
  ("fixed", FixedArg),
  ("string", StringArg),
  ("fd", FdArg)]

-- struct wl_message {
--         const char *name;
--         const char *signature;
--         const struct wl_interface **types;
-- };

type Argument = (String, ArgumentType, Bool) -- name, argument type, allow-null
data Message = Message {
  messageName :: String,
  messageArguments :: [Argument]
  } deriving (Show)

data ProtocolSpec = ProtocolSpec {
  specInterfaces :: [Interface]
  } deriving (Show)

readProtocol :: IO ProtocolSpec
readProtocol = do
  datadir <- figureOutWaylandDataDir
  protocolXmlFile <- readFile (datadir ++ "/" ++ protocolFile)
  let xmlTree = (!!1) $ onlyElems $ parseXML protocolXmlFile :: Element


  -- define some tags
  let interface  = QName "interface"  Nothing Nothing
      request    = QName "request"    Nothing Nothing
      event      = QName "event"      Nothing Nothing
      enum       = QName "enum"       Nothing Nothing
      entry      = QName "entry"      Nothing Nothing
      arg        = QName "arg"        Nothing Nothing
      namexml    = QName "name"       Nothing Nothing
      version    = QName "version"    Nothing Nothing
      allow_null = QName "allow-null" Nothing Nothing
      typexml    = QName "type"       Nothing Nothing
      value      = QName "value"      Nothing Nothing

  -- This is where we parse the XML... i.e. the magic of this file.
  let interfaces = map parseInterface $ findChildren interface xmlTree where
        parseInterface elt = Interface {
          interfaceName = fromJust $ findAttr namexml elt,
          interfaceVersion = read $ fromJust $ findAttr version elt, -- unused atm
          interfaceMethods = catMaybes $ map parseMessage $ findChildren request elt,
          interfaceEvents = catMaybes $ map parseMessage $ findChildren event elt,
          interfaceEnums = map parseEnum $ findChildren enum elt
          } where
            parseMessage :: Element -> Maybe Message -- we're gonna do some fancy construction to skip messages we can't deal with yet
            parseMessage msgelt = do
              let name = fromJust $ findAttr namexml msgelt
              arguments <- sequence $ map parseArgument $ findChildren arg msgelt
              return $ Message {messageName = name, messageArguments = arguments} where
                parseArgument argelt = do
                  let name = fromJust $ findAttr namexml argelt
		  let argtypecode = fromJust $ findAttr typexml argelt
                  argtype <- case argtypecode of
                    "object" -> ObjectArg <$> findAttr interface argelt
                    "new_id" -> NewIdArg <$> findAttr interface argelt
                    _ -> lookup argtypecode argConversionTable
                  let allowNull = fromMaybe False (read <$> capitalize <$> findAttr allow_null argelt)
                  return (name, argtype, allowNull)
            parseEnum enumelt =
              let enumname = fromJust $ findAttr namexml enumelt
                  entries = map parseEntry $ findChildren entry enumelt
              in WLEnum {enumName = enumname, enumEntries = entries} where
                parseEntry entryelt = (fromJust $ findAttr namexml entryelt,
                                       read $ fromJust $ findAttr value entryelt :: Int)
  return $ ProtocolSpec interfaces

generateTypes :: ProtocolSpec -> [Dec]
generateTypes ps = map generateInterface (specInterfaces ps) where
  generateInterface iface =
    let qname = mkName $ prettyInterfaceName $ interfaceName iface
    in
      (NewtypeD [] qname [] (NormalC qname [(NotStrict,AppT (ConT ''Ptr) (ConT qname))]) [])

-- generateClientMethods :: ProtocolSpec -> [Dec]

-- generateServerMethods :: ProtocolSpec -> [Dec]


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
capitalize :: String -> String
capitalize x = toUpper (head x) : tail x

prettyInterfaceName :: String -> String
prettyInterfaceName = capitalize . toCamel . removeInitial "wl_"

prettyMessageName :: String -> String -> String
prettyMessageName ifacename msgname = toCamel $ ((removeInitial "wl_" ifacename) ++ "_" ++ msgname)

figureOutWaylandDataDir :: IO String
figureOutWaylandDataDir =
  head <$> lines <$> readProcess "pkg-config" ["wayland-server", "--variable=pkgdatadir"] []

protocolFile = "wayland.xml"
