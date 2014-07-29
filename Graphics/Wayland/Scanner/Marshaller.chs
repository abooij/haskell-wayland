{-# LANGUAGE TemplateHaskell #-}
module Graphics.Wayland.Scanner.Marshaller where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Process
import System.IO
import System.Posix.Types
import Language.Haskell.TH

import Graphics.Wayland.Scanner.Protocol
import Graphics.Wayland.Scanner.Names

#include <wayland-server.h>

{#context prefix="wl"#}



argTypeToType :: ArgumentType -> TypeQ
argTypeToType IntArg = [t| {#type int32_t#} |]
argTypeToType UIntArg = [t| {#type uint32_t#} |]
argTypeToType FixedArg = [t|{#type fixed_t#}|]
argTypeToType StringArg = [t| Ptr CChar |]
argTypeToType (ObjectArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToType (NewIdArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToType ArrayArg = undefined
argTypeToType FdArg = [t| {#type int32_t#} |]

argTypeToHaskType :: ArgumentType -> TypeQ
argTypeToHaskType IntArg = [t|Int|]
argTypeToHaskType UIntArg = [t|Word|]
argTypeToHaskType FixedArg = [t|Int|] -- FIXME double conversion!!
argTypeToHaskType StringArg = [t|String|]
argTypeToHaskType (ObjectArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToHaskType (NewIdArg iname) = return $ ConT $ interfaceTypeName iname
argTypeToHaskType ArrayArg = undefined
argTypeToHaskType FdArg = [t|Fd|]

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

-- | Opposite of argTypeMarshaller.
argTypeUnmarshaller :: [Argument] -> ExpQ -> ([Pat], ExpQ)
argTypeUnmarshaller args fun =
  let vars = map marshallerVar args
      mk = return . VarE . marshallerVar
      applyUnmarshaller :: [Argument] -> ExpQ -> ExpQ
      applyUnmarshaller (arg@(_, IntArg, _):as) fun = [|$(applyUnmarshaller as [|$fun (fromIntegral ($(mk arg) :: CInt) )|])|]
      applyUnmarshaller (arg@(_, UIntArg, _):as) fun = [|$(applyUnmarshaller as [|$fun (fromIntegral ($(mk arg) :: CUInt))|]) |]
      applyUnmarshaller (arg@(_, FixedArg, _):as) fun = [|$(applyUnmarshaller as [|$fun (fromIntegral ($(mk arg) :: CInt))|]) |] -- FIXME double conversion stuff!
      applyUnmarshaller (arg@(_, StringArg, _):as) fun = [|do str <- peekCString $(mk arg); $(applyUnmarshaller as [|$fun str|])|]
      applyUnmarshaller (arg@(_, (ObjectArg iname), _):as) fun = [|$(applyUnmarshaller as [|$fun $(mk arg)|]) |] -- FIXME Maybe
      applyUnmarshaller (arg@(_, (NewIdArg iname), _):as) fun = [|$(applyUnmarshaller as [|$fun $(mk arg) |])|] -- FIXME Maybe
      applyUnmarshaller (arg@(_, ArrayArg, _):as) fun = undefined
      applyUnmarshaller (arg@(_, FdArg, _):as) fun = [|$(applyUnmarshaller as [|$fun (Fd ($(mk arg)))|]) |]
      applyUnmarshaller [] fun = fun
  in  (map VarP vars, applyUnmarshaller args fun)


-- compute FunPtr size and alignment based on some dummy C type
funcSize = {#sizeof notify_func_t#} :: Integer
funcAlign = {#alignof notify_func_t#} :: Integer
