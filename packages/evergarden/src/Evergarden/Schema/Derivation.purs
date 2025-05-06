-- | This module defines some type-level machineries for deriving  
--  the type of app from given schema (assumed to be already normalized). 
module Evergarden.Schema.Derivation where

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Tuple (Tuple)
import Evergarden.Json.Engine (JsonDecoder, JsonEncoder, JsonEngine)
import Evergarden.Schema (Scope)
import Evergarden.Schema.Endpoint (class IsNoInput, Endpoint, NoInput)
import Evergarden.Schema.Operations (class GetScopedOperation, class GetScopedOperations)
import Evergarden.Schema.Toplevel (class SchemaOperations, class SchemaResource)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (Text)
import Routing.Duplex (RouteDuplex')
import Type.Data.Boolean (class If)
import Type.Data.Function (class Curry)
import Type.Data.List as TList
import Type.Data.Maybe (class FromJustOrFail, class FromMaybe)
import Type.Equality (class TypeEquals)
import Type.Extra.Row (class GetAt)
import Type.Proxy (Proxy(..))

class DeriveApp
  :: Type
  -> (Type -> Type)
  -> Type
  -> Type
  -> Constraint
class DeriveApp scm m json app | scm m json -> app

instance
  ( SchemaResource scm resource
  , SchemaOperations scm (Record opsR)
  , RowToList opsR opsL
  , DeriveAppRowList scm m json opsL appOpsR
  , TypeEquals
      appR
      ( operations :: Record appOpsR
      , resources :: RouteDuplex' resource
      , jsonEngine :: JsonEngine json
      )
  ) =>
  DeriveApp scm m json (Record appR)

class DeriveAppRowList
  :: Type
  -> (Type -> Type)
  -> Type
  -> RowList Type
  -> Row Type
  -> Constraint
class DeriveAppRowList scm m json opsL app | scm opsL -> app

instance DeriveAppRowList scm m json RL.Nil ()
instance
  ( DeriveAppRowList scm m json tl restR
  , DeriveScopedAppType scm m json scope sapp
  , Row.Cons scope sapp restR appR
  ) =>
  DeriveAppRowList scm m json (RL.Cons scope sops tl) appR

deriveAppTest :: forall @scm @m @json app. DeriveApp scm m json app => Proxy app
deriveAppTest = Proxy

class DeriveScopedAppType :: Type -> (Type -> Type) -> Type -> Symbol -> Type -> Constraint
class DeriveScopedAppType scm m json scope app | scm m json scope -> app

instance
  ( GetScopedOperations scm scope (Scope pfx ext sops)
  , TypeEquals (Record sopsR) sops
  , RowToList sopsR sopsL
  , DeriveScopedAppTypeRowList scm m json scope sopsL () appHandlersR
  , TypeEquals appR
      ( handlers :: Record appHandlersR 
      )
  ) =>
  DeriveScopedAppType scm m json scope (Record appR)

class DeriveScopedAppTypeRowList
  :: Type
  -> (Type -> Type)
  -> Type
  -> Symbol
  -> RowList Type
  -> Row Type
  -> Row Type
  -> Constraint
class DeriveScopedAppTypeRowList schema m json scope sopsL acc app | schema m json scope sopsL acc -> app

instance DeriveScopedAppTypeRowList scm m json scope RL.Nil app app

instance
  ( DeriveScopedAppTypeRowList scm m json scope tl acc rest
  , DeriveScopedAppHandlerType scm m json scope opid appHandlerR
  , Row.Cons opid appHandlerR rest appR
  ) =>
  DeriveScopedAppTypeRowList scm m json scope (RL.Cons opid endpoint tl) acc appR

class DeriveScopedAppHandlerType
  :: Type 
  -> (Type -> Type)
  -> Type 
  -> Symbol 
  -> Symbol
  -> Type 
  -> Constraint
class DeriveScopedAppHandlerType scm m json scope opid appHandlerR | scm m json scope opid -> appHandlerR

instance 
  ( MakeScopedOperationSignature scm scope opid params input output
  , IsNoInput input isNoInput
  , If isNoInput (m output) (input -> m output) ret
  , Curry (Function params ret) funcType
  , If isNoInput () (decoder :: JsonDecoder json input) decoderRec
  , TypeEquals 
      handlerEncoderRec
      ( encoder :: JsonEncoder output json
      , handler :: funcType
      )
  , Row.Union handlerEncoderRec decoderRec appHandlerR
  ) =>
  DeriveScopedAppHandlerType scm m json scope opid (Record appHandlerR)

class FoldParamListToListType :: Type -> TList.List Type -> TList.List Type -> Constraint
class FoldParamListToListType params ret fn | params ret -> fn

instance FoldParamListToListType NoInput ret ret

else instance
  ( FoldParamListToListType b ret fn'
  ) =>
  FoldParamListToListType (Tuple a b) ret (TList.Cons a fn')

else instance
  FoldParamListToListType a ret (TList.Cons a ret)

class ConsIfnNoInput :: forall k. k -> TList.List k -> TList.List k -> Constraint
class ConsIfnNoInput t xs ys | t xs -> ys

instance ConsIfnNoInput NoInput xs xs
else instance ConsIfnNoInput t xs (TList.Cons t xs)

makeScopedAppTest :: forall @scm @scope @m @json app. DeriveScopedAppType scm m json scope app => Proxy app
makeScopedAppTest = Proxy

class MakeScopedOperationSignature
  :: Type
  -- schema type 
  -> Symbol
  -- ^ scope id
  -> Symbol
  -- ^ operation id
  -> Type
  -- ^ route parameters (Tuple if it has multiple parameters)
  -> Type
  -- ^ handler input type
  -> Type
  -- ^ handler output type
  -> Constraint
class MakeScopedOperationSignature schema scope op params args ret | schema scope op -> params args ret

instance
  ( SchemaResource schema resource
  , GetScopedOperation schema scope opid endpoint
  , TypeEquals (Endpoint method path infs) endpoint
  , RouteParameters resource path params
  , GetEndpointInput endpoint input
  , GetEndpointOutput endpoint output
  ) =>
  MakeScopedOperationSignature schema scope opid params input output

class RouteParameters :: Type -> Symbol -> Type -> Constraint
class RouteParameters resource path params | resource path -> params

instance
  ( Generic resource resourceRep
  , RouteRepParameters resourceRep path params
  ) =>
  RouteParameters resource path params

class RouteRepParameters :: Type -> Symbol -> Type -> Constraint
class RouteRepParameters resourceRep path params | resourceRep path -> params

instance
  ( ConstructorArgParams args params
  ) =>
  RouteRepParameters (Constructor path args) path params

instance
  ( ConstructorArgParams args params
  ) =>
  RouteRepParameters (Sum (Constructor path args) r) path params

else instance
  ( RouteRepParameters r path params
  ) =>
  RouteRepParameters (Sum l r) path params

class ConstructorArgParams :: Type -> Type -> Constraint
class ConstructorArgParams args params | args -> params

instance ConstructorArgParams NoArguments NoInput
instance ConstructorArgParams (Argument t) t
instance
  ( ConstructorArgParams l p1
  , ConstructorArgParams r p2
  ) =>
  ConstructorArgParams (Product l r) (Tuple p1 p2)

class GetEndpointInput :: Type -> Type -> Constraint
class GetEndpointInput endpoint input | endpoint -> input

instance
  ( TypeEquals (Record infR) infs
  , GetAt infR "input" mbInput
  , FromMaybe NoInput mbInput input
  ) =>
  GetEndpointInput (Endpoint method path infs) input

class GetEndpointOutput :: Type -> Type -> Constraint
class GetEndpointOutput endpoint output | endpoint -> output

instance
  ( TypeEquals (Record infR) infs
  , GetAt infR "output" mbOutput
  , FromJustOrFail
      (Text "You must specify output type")
      mbOutput
      output
  ) =>
  GetEndpointOutput (Endpoint method path infs) output

makeScopedOperationsSignatureTest
  :: forall @schema @scope @op params input output
   . MakeScopedOperationSignature schema scope op params input output
  => Proxy params
  -> Proxy input
  -> Proxy output
makeScopedOperationsSignatureTest = \_ _ -> Proxy
