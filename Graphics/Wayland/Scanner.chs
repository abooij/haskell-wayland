{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Scanner where

import Data.Functor
import Control.Monad (liftM)
import Foreign
import Foreign.C.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)

import Graphics.Wayland.Scanner.Marshaller
import Graphics.Wayland.Scanner.Names
import Graphics.Wayland.Scanner.Protocol

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
      in forImpD cCall unsafe cname hname (genMessageCType msg)

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

generateListenersExternal :: ProtocolSpec -> ServerClient -> Q [Dec]
generateListenersExternal sp sc = liftM concat $ sequence $ map (\iface -> generateListenerExternal iface sc)  $ specInterfaces sp

generateClientListenersExternal sp = generateListenersExternal sp Client
generateServerListenersExternal sp = generateListenersExternal sp Server

generateListenerExternal :: Interface -> ServerClient -> Q [Dec]
generateListenerExternal iface sc =
  let -- declare a Listener or Interface type for this interface
      typeName :: Name
      typeName = case sc of
                       Server -> mkName $ (prettyInterfaceName $ iname ++ "Interface")
                       Client -> mkName $ (prettyInterfaceName $ iname ++ "Listener")
      iname :: String
      iname = interfaceName iface
      messages :: [Message]
      messages = case sc of
                   Server -> interfaceMethods iface
                   Client -> interfaceEvents iface
      mkMessageName :: Message -> Name
      mkMessageName msg = case sc of
                       Server -> mkName $ getRequestHaskName iname (messageName msg)
                       Client -> mkName $ getEventHaskName iname (messageName msg)
      mkListener :: Message -> VarStrictTypeQ
      mkListener event = do
        let name = mkMessageName event
        ltype <- mkListenerType event
        return (name, NotStrict, ltype)
      listenerType :: DecQ
      listenerType = do
        recArgs <- sequence $ map mkListener messages
        return $ DataD [] typeName [] [RecC typeName recArgs] []
      mkListenerType :: Message -> TypeQ
      mkListenerType event = genMessageHaskType event
      mkListenerCType event = genMessageCType event

      -- instance dec: this struct better be Storable
      instanceDec :: DecsQ
      instanceDec = do
        instanceName <- [t|Storable $(return $ ConT typeName)|]
        -- instanceDecs <- [d|
        --   sizeOf _    = $(return $ LitE $ IntegerL (funcSize * (fromIntegral $ length messages)))
        --   alignment _ = $(return $ LitE $ IntegerL funcAlign)
        --   peek _ = undefined
        --   poke _ _ = undefined
        --   |]
        let numNewIds msg = sum $ map isNewId $ messageArguments msg
            isNewId arg = case arg of
                (_, NewIdArg _, _) -> 1
                _                  -> 0
            fixedArgs msg = if numNewIds msg == 1
                then filter notNewIds $ messageArguments msg
                else messageArguments msg
            notNewIds arg = case arg of
                (_, NewIdArg _, _) -> False
                _                  -> True
        [d|instance Storable $(conT typeName) where
            sizeOf _ = $(litE $ IntegerL $ funcSize * (fromIntegral $ length messages))
            alignment _ = $(return $ LitE $ IntegerL funcAlign)
	    peek _ = undefined  -- we shouldn't need to be able to read listeners (since we can't change them anyway)
	    poke ptr record = $(doE $ ( zipWith (\ event idx ->
                noBindS [e|do
                  let haskFun = $(return $ VarE $ mkMessageName event) record
                      unmarshaller fun = $(let (pats, funexp) = argTypeUnmarshaller (fixedArgs event) (return $ VarE 'fun)
                                           in LamE pats <$> funexp)

                  funptr <- $(return $ (VarE $ wrapperName event)) (unmarshaller haskFun)
                  -- funptr <- $(return $ AppE (VarE $ wrapperName event) (AppE (VarE $ mkMessageName event) (VarE 'record)))
                  pokeByteOff ptr $(litE $ IntegerL (idx * funcSize)) funptr
                |] )
              messages [0..]
              ) ++ [noBindS [e|return () |]] )
            |]


      -- FunPtr wrapper
      wrapperName event = mkName $ prettyMessageName iname (messageName event ++ "_wrapper")
      wrapperDec event = forImpD CCall Unsafe "wrapper" (wrapperName event) [t|$(mkListenerCType event) -> IO (FunPtr ($(mkListenerCType event))) |]

      -- bind add_listener
      foreignName = mkName $ prettyMessageName iname "c_add_listener"
      haskName = mkName $ prettyMessageName iname "add_listener"
      foreignDec :: Q Dec
      foreignDec = forImpD CCall Unsafe (iname ++ "_add_listener") foreignName [t|$(conT $ mkName $ prettyInterfaceName iname) -> (Ptr $(conT $ typeName))  -> (Ptr ()) -> CInt|]
      apiDec :: Q [Dec]
      apiDec = [d|$(return $ VarP $ mkName $ prettyMessageName iname "add_listener") = \ iface listener ->
                   do
                    -- malloc RAM for Listener type
                    memory <- malloc
                    -- store Listener type
                    poke memory listener
                    -- call foreign add_listener on stored Listener type
                    return $ 0 == $(return $ VarE $ foreignName) iface memory nullPtr
                    |]

      -- apiDec = [d|$(return $ VarP (mkName "bla")) listener = return|]

  in do
    some <- sequence $ listenerType : map wrapperDec messages
    other <- instanceDec
    more <- foreignDec
    last <- apiDec
    return $ some ++ other ++ [more] ++ last


generateListenersInternal :: ProtocolSpec -> ServerClient -> Q [Dec]
generateListenersInternal sp sc = liftM concat $ sequence $ map (\iface -> generateListenerInternal iface sc)  $ specInterfaces sp

generateClientListenersInternal sp = generateListenersInternal sp Client
generateServerListenersInternal sp = generateListenersInternal sp Server

generateListenerInternal :: Interface -> ServerClient -> Q [Dec]
generateListenerInternal iface sc = undefined


generateClientInternalMethods :: ProtocolSpec -> Q [Dec]
generateClientInternalMethods ps = generateInternalMethods ps Client

generateServerInternalMethods :: ProtocolSpec -> Q [Dec]
generateServerInternalMethods ps = generateInternalMethods ps Server

generateClientExternalMethods :: ProtocolSpec -> Q [Dec]
generateClientExternalMethods ps = generateExternalMethods ps Client

generateServerExternalMethods :: ProtocolSpec -> Q [Dec]
generateServerExternalMethods ps = generateExternalMethods ps Server

genMessageCType :: Message -> TypeQ
genMessageCType = genMessageType argTypeToType

genMessageHaskType :: Message -> TypeQ
genMessageHaskType = genMessageType argTypeToHaskType

genMessageType :: (ArgumentType -> TypeQ) -> Message -> TypeQ
genMessageType fun msg =
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
                    then fun $ snd3 $ head $ filter (not.notNewIds) $ messageArguments msg
                    else [t|()|]
  in
    foldr (\addtype curtype -> [t|$addtype -> $curtype|]) [t|IO $(returnType)|] $ (map (fun.snd3) fixedArgs)
