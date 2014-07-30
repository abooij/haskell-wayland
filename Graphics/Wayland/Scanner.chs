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
import Graphics.Wayland.Scanner.Types

generateTypes :: ProtocolSpec -> [Dec]
generateTypes ps = map generateInterface (protocolInterfaces ps) where
  generateInterface :: Interface -> Dec
  generateInterface iface =
    let qname = interfaceTypeName (protocolName ps) (interfaceName iface)
    in
      (NewtypeD [] qname [] (NormalC qname [(NotStrict,AppT (ConT ''Ptr) (ConT qname))]) [mkName "Show"])

generateEnums :: ProtocolSpec -> [Dec]
generateEnums ps = concat $ map eachGenerateEnums (protocolInterfaces ps) where
  eachGenerateEnums :: Interface -> [Dec]
  eachGenerateEnums iface = concat $ map generateEnum $ interfaceEnums iface where
    generateEnum :: WLEnum -> [Dec]
    generateEnum wlenum =
      let qname = enumTypeName (protocolName ps) (interfaceName iface) (enumName wlenum)
      in
        NewtypeD [] qname [] (NormalC qname [(NotStrict, (ConT ''Int))]) [mkName "Show", mkName "Eq"]
        :
        map (\(entry, val) -> (ValD (VarP $ enumEntryHaskName (protocolName ps) (interfaceName iface) (enumName wlenum) entry) (NormalB $ AppE (ConE qname) $ LitE $ IntegerL $ toInteger val) [])) (enumEntries wlenum)

-- | generate FFI for a certain side of the API
generateInternalMethods :: ProtocolSpec -> ServerClient -> Q [Dec]
generateInternalMethods ps sc = liftM concat $ sequence $ map generateInterface $ filter (\iface -> if sc == Server then interfaceName iface /= "wl_display" else True) $ protocolInterfaces ps where
  generateInterface :: Interface -> Q [Dec]
  generateInterface iface = sequence $ map generateMessage $ case sc of
                                                               Server -> interfaceEvents iface
                                                               Client -> interfaceRequests iface
                                                               where
    generateMessage :: Message -> Q Dec
    generateMessage msg =
      let pname = protocolName ps
          iname = interfaceName iface
          mname = messageName msg
          -- name of C function as in the header files
          cname = case sc of
                    Server -> eventForeignCName Server iname mname
                    Client -> requestForeignCName iname mname
          -- "dirty" name of internal raw binding to C function
          hname = case sc of
                    Server -> eventInternalCName iname mname
                    Client -> requestInternalCName iname mname
      in forImpD cCall unsafe cname hname (genMessageCType msg)

generateExternalMethods :: ProtocolSpec -> ServerClient -> Q [Dec]
generateExternalMethods ps sc = liftM concat $ sequence $ map generateInterface $ filter (\iface -> if sc == Server then interfaceName iface /= "wl_display" else True) $ protocolInterfaces ps where
  generateInterface :: Interface -> Q [Dec]
  generateInterface iface = liftM concat $ sequence $ map generateMessage $ if sc == Server then interfaceEvents iface else interfaceRequests iface where
    generateMessage :: Message -> Q [Dec]
    generateMessage msg =
      let iname = interfaceName iface
          mname = messageName msg
          pname = protocolName ps
          -- name of haskell foreign bind to c function
          cname = case sc of
                    Server -> eventInternalCName iname mname
                    Client -> requestInternalCName iname mname
          -- name of pretty Haskell API that we're exposing
          hname = case sc of
                    Server -> eventHaskName pname iname mname
                    Client -> requestHaskName pname iname mname
      in do
         let funexp = return $ VarE cname
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
         let (pats, fun) = argTypeMarshaller fixedArgs funexp
         gens <- [d|$(return $ VarP hname) = $(LamE pats <$> fun) |]
         return gens

generateListenersExternal :: ProtocolSpec -> ServerClient -> Q [Dec]
generateListenersExternal sp sc = liftM concat $ sequence $ map (\iface -> generateListenerExternal sp iface sc)  $ protocolInterfaces sp

generateClientListenersExternal sp = generateListenersExternal sp Client
generateServerListenersExternal sp = generateListenersExternal sp Server

generateListenerExternal :: ProtocolSpec -> Interface -> ServerClient -> Q [Dec]
generateListenerExternal sp iface sc =
  let -- declare a Listener or Interface type for this interface
      typeName :: Name
      typeName = messageListenerTypeName sc (protocolName sp) (interfaceName iface)
      pname = protocolName sp
      iname :: String
      iname = interfaceName iface
      messages :: [Message]
      messages = case sc of
                   Server -> interfaceRequests iface
                   Client -> interfaceEvents iface
      mkMessageName :: Message -> Name
      mkMessageName msg = case sc of
                       Server -> eventHaskName pname iname (messageName msg)
                       Client -> requestHaskName pname iname (messageName msg)
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
      wrapperName event = messageListenerWrapperName sc iname (messageName event)
      wrapperDec event = forImpD CCall Unsafe "wrapper" (wrapperName event) [t|$(mkListenerCType event) -> IO (FunPtr ($(mkListenerCType event))) |]

      -- bind add_listener
      haskName = requestHaskName pname iname "add_listener" -- FIXME this only works for the client. Also dunno why I can't use this variable in the splice below.
      foreignName = requestInternalCName iname "c_add_listener"
      foreignDec :: Q Dec
      foreignDec = forImpD CCall Unsafe (iname ++ "_add_listener") foreignName [t|$(conT $ interfaceTypeName pname iname) -> (Ptr $(conT $ typeName))  -> (Ptr ()) -> CInt|]
      apiDec :: Q [Dec]
      apiDec = [d|$(varP $ requestHaskName (protocolName sp) iname "add_listener") = \ iface listener ->
                   do
                    -- malloc RAM for Listener type
                    memory <- malloc
                    -- store Listener type
                    poke memory listener
                    -- call foreign add_listener on stored Listener type
                    return $ 0 == $(varE foreignName) iface memory nullPtr
                    |]

      -- apiDec = [d|$(return $ VarP (mkName "bla")) listener = return|]

  in do
    some <- sequence $ listenerType : map wrapperDec messages
    other <- instanceDec
    more <- foreignDec
    last <- apiDec
    return $ some ++ other ++ [more] ++ last


generateListenersInternal :: ProtocolSpec -> ServerClient -> Q [Dec]
generateListenersInternal sp sc = liftM concat $ sequence $ map (\iface -> generateListenerInternal iface sc)  $ protocolInterfaces sp

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
genMessageCType = genMessageType argTypeToCType

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



-- | 3-tuple version of snd
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
