module Graphics.Wayland.Scanner.Protocol (
  readProtocol
  ) where

import Data.Functor
import Data.Maybe
import Data.Char
import Text.XML.Light
import System.Process

import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.Names

interface  = QName "interface"  Nothing Nothing
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

parseInterface :: ProtocolName -> Element -> Interface
parseInterface pname elt =
  let iname = fromJust $ findAttr namexml elt
      parseMessage :: Element -> Maybe Message
      parseMessage msgelt = do -- we're gonna do some fancy construction to skip messages we can't deal with yet
        let name = fromJust $ findAttr namexml msgelt
        arguments <- mapM parseArgument (findChildren arg msgelt)
        return Message {messageName = name, messageArguments = (iname, ObjectArg $ interfaceTypeName pname iname, False) : arguments} where
          parseArgument argelt = do
            let msgname = fromJust $ findAttr namexml argelt
            let argtypecode = fromJust $ findAttr typexml argelt
            argtype <- case argtypecode of
              "object" -> ObjectArg . interfaceTypeName pname <$> findAttr interface argelt
              "new_id" -> NewIdArg . interfaceTypeName pname <$> findAttr interface argelt
              _ -> lookup argtypecode argConversionTable
            let allowNull = fromMaybe False (read <$> capitalize <$> findAttr allow_null argelt)
            return (msgname, argtype, allowNull)
      parseEnum enumelt =
        let enumname = fromJust $ findAttr namexml enumelt
            entries = map parseEntry $ findChildren entry enumelt
        in WLEnum {enumName = enumname, enumEntries = entries} where
          parseEntry entryelt = (fromJust $ findAttr namexml entryelt,
                                 read $ fromJust $ findAttr value entryelt :: Int)
  in Interface {
     interfaceName = iname,
     interfaceVersion = read $ fromJust $ findAttr version elt, -- unused atm
     interfaceRequests = mapMaybe parseMessage (findChildren request elt),
     interfaceEvents = mapMaybe parseMessage (findChildren event elt),
     interfaceEnums = map parseEnum $ findChildren enum elt
     }

-- | locate wayland.xml on disk and parse it
readProtocol :: IO ProtocolSpec
readProtocol = do
  datadir <- figureOutWaylandDataDir
  protocolXmlFile <- readFile (datadir ++ "/" ++ protocolFile)
  let xmlTree = (!!1) $ onlyElems $ parseXML protocolXmlFile :: Element

  let pname = fromJust $ findAttr namexml xmlTree
  -- This is where we parse the XML... i.e. the magic of this file.
  let interfaces = map (parseInterface pname) $ findChildren interface xmlTree
  return $ ProtocolSpec pname interfaces

-- TODO move this into some pretty Setup.hs thing
figureOutWaylandDataDir :: IO String
figureOutWaylandDataDir =
  head <$> lines <$> readProcess "pkg-config" ["wayland-server", "--variable=pkgdatadir"] []

protocolFile = "wayland.xml"
