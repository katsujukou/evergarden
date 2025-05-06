-- | This module defines some typelevel-machineries for
--   mapping HTTP requests (method * resource path) to 
--   corresponding operation id according to the given schema. 
--  
--   E.g.  GET "Todos"    --> listTodos
--         POST "Todos"   --> createTodos
--         GET "Todo"     --> getTodo       
--         and so on.
module Evergarden.Schema.Routing where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Reflectable (class Reflectable, reflectType)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Evergarden.Schema (Scope)
import Evergarden.Schema.Derivation (class DeriveApp, class DeriveScopedAppHandlerType)
import Evergarden.Schema.Endpoint (Endpoint, Scoped)
import Evergarden.Schema.Method (Method)
import Evergarden.Schema.Operations (class GetScopedOperation)
import Evergarden.Schema.Routing.RouteParams (class RouteParams, AnyParam, routeParams, unsafeApplyParams)
import Evergarden.Schema.Toplevel (class SchemaOperations, class SchemaResource)
import HTTPurple as HTTPurple
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Unsafe (unsafeGet)
import Type.Data.List as TList
import Type.Data.Maybe as TMaybe
import Type.Data.Tuple (type (*), KTuple)
import Type.Data.Tuple as TTuple
import Type.Equality (class TypeEquals)
import Type.Extra.Row (class GetAt)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Type-level map, represented as a list of key-value pairs.
type TMap :: forall k1 k2. k1 -> k2 -> Type
type TMap k t = TList.List (KTuple k t)

infixr 5 type TMap as :=>

-- | A kind for type-level scoped routing tables, which associates
--   pairs of (method * resource path (symbol)) to scoped operation ids.
type KScopedRouteTable = (Method * Symbol) :=> Type

-- | A kind for combined routing tables.
type KRouteTable = Symbol :=> KScopedRouteTable

class RegisterRoutes
  :: Type
  -> (Type -> Type)
  -> Type
  -> Type
  -> Type
  -> Constraint
class
  RegisterRoutes schema m json app resource
  | schema m json -> app resource
  where
  registerRoutes
    :: Proxy schema
    -> Proxy json
    -> app
    -> Aff
         ( HashMap
             String
             ( HashMap
                 (String /\ String)
                 (HTTPurple.Request resource -> m HTTPurple.Response)
             )
         )

instance
  ( Monad m
  , DeriveApp schema m json app
  , SchemaResource schema resource
  , MakeResourceOperationMap schema map
  , RegisterRoutesPerScope map schema m json resource app
  ) =>
  RegisterRoutes schema m json app resource where
  registerRoutes pscm pjson app = do
    registerRoutesPerScope (Proxy :: _ map) pscm pjson app

class RegisterRoutesPerScope
  :: KRouteTable
  -> Type
  -> (Type -> Type)
  -> Type
  -> Type
  -> Type
  -> Constraint
class
  RegisterRoutesPerScope map schema m json resource app
  | schema map m json -> resource app
  where
  registerRoutesPerScope
    :: Proxy map
    -> Proxy schema
    -> Proxy json
    -> app
    -> Aff
         ( HashMap
             String
             -- ^ scope id
             ( HashMap
                 (String /\ String)
                 -- ^ method * resource path
                 (HTTPurple.Request resource -> m HTTPurple.Response)
             -- ^ route handler function
             )
         )

instance
  ( Monad m
  ) =>
  RegisterRoutesPerScope TList.Nil schema m json resource app
  where
  registerRoutesPerScope _ _ _ _ = pure HM.empty

instance
  ( SchemaResource schema resource
  , Monad m
  , DeriveApp schema m json app
  , TypeEquals (TList.Cons mapHd mapTl) map
  , TypeEquals (TTuple.Tuple scope scopedMap) mapHd
  , RegisterScopedRoutes schema scope scopedMap m json resource app
  , RegisterRoutesPerScope mapTl schema m json resource app
  , Reflectable scope String
  ) =>
  RegisterRoutesPerScope (TList.Cons mapHd mapTl) schema m json resource app
  where
  registerRoutesPerScope _ pscm pjson app = do
    scopedMapTl <- registerRoutesPerScope (Proxy :: _ mapTl) pscm pjson app
    scopedMapHd <- registerScopedRoutes
      pscm
      (Proxy :: _ scope)
      (Proxy :: _ scopedMap)
      pjson
      app
    pure $
      HM.insert
        (reflectType (Proxy :: _ scope))
        scopedMapHd
        scopedMapTl

class RegisterScopedRoutes
  :: Type
  -> Symbol
  -> KScopedRouteTable
  -> (Type -> Type)
  -> Type
  -> Type
  -> Type
  -> Constraint
class
  RegisterScopedRoutes schema scope scopedMap m json resource app
  | schema scope scopedMap m json -> resource app
  where
  registerScopedRoutes
    :: Proxy schema
    -> Proxy scope
    -> Proxy scopedMap
    -> Proxy json
    -> app
    -> Aff
         ( HashMap
             (String /\ String)
             (HTTPurple.Request resource -> m HTTPurple.Response)
         )

instance
  ( Monad m
  , DeriveApp schema m json app
  , SchemaResource schema resource
  ) =>
  RegisterScopedRoutes schema scope TList.Nil m json resource app
  where
  registerScopedRoutes _ _ _ _ _ = pure $ HM.empty

instance
  ( DeriveApp schema m json app
  , SchemaResource schema resource
  , RouteParams resource
  , MonadAff m
  , RegisterScopedRoutes schema scope smapTl m json resource app
  , TypeEquals (TTuple.Tuple methodResource sopid) smapHd
  , TypeEquals (TTuple.Tuple method resourcePath) methodResource
  , TypeEquals (Scoped scope opid) sopid
  , GetScopedOperation schema scope opid endpoint
  , DeriveScopedAppHandlerType schema m json scope opid handlerRec
  , TypeEquals (Record handlerRecR) handlerRec
  , GetAt handlerRecR "decoder" mbDecoder
  , TMaybe.IsNothing mbDecoder hasNoInput
  , Reflectable hasNoInput Boolean
  , Reflectable scope String
  , Reflectable opid String
  , Reflectable method HTTPurple.Method
  , Reflectable resourcePath String
  ) =>
  RegisterScopedRoutes schema scope (TList.Cons smapHd smapTl) m json resource app
  where
  registerScopedRoutes pscm pscope _ pjson app' = do
    tl <- registerScopedRoutes pscm pscope (Proxy :: _ smapTl) pjson app'

    let
      scope = reflectType (Proxy :: _ scope)
      opid = reflectType (Proxy :: _ opid)
      app = (unsafeCoerce app' :: Record _)
      jsonEngine = Record.get (Proxy :: _ "jsonEngine") app
      opHandlerRec = app
        # unsafeGet "operations"
        # unsafeGet scope
        # unsafeGet "handlers"
        # unsafeGet opid
      handler = unsafeGet "handler" opHandlerRec
    --
    let
      routerFn :: HTTPurple.Request resource -> m HTTPurple.Response
      routerFn req = do
        let
          route = req.route
          params = routeParams route
        -- When the handler takes input  
        mbInput <- case reflectType (Proxy :: _ hasNoInput) of
          true -> pure $ Right []
          _ -> do
            body <- HTTPurple.toString req.body
            let
              jsonDecoder :: String -> Either String json
              jsonDecoder = unsafeGet "deserialize" jsonEngine

              decode :: String -> Either String AnyParam
              decode = jsonDecoder >=> (unsafeGet "decoder" opHandlerRec)
            pure $ Array.singleton <$> decode body

        case mbInput of
          Left decodeError -> HTTPurple.badRequest decodeError
          Right inps -> do
            let
              handlerBody = unsafeApplyParams handler (params <> inps)
            output <- handlerBody

            -- Encode and serialize output and return as HTTPurple response. 
            let
              jsonEncoder :: json -> String
              jsonEncoder = unsafeGet "serialize" jsonEngine

            HTTPurple.ok $ jsonEncoder $ (unsafeGet "encoder" opHandlerRec) output

    let
      method = reflectType (Proxy :: _ method)
      resourcePath = reflectType (Proxy :: _ resourcePath)
      routeTbl =
        HM.insert
          ( Tuple
              (show method)
              resourcePath
          )
          routerFn
          tl

    Console.log $
      "\x1b[32mRoute mapped:\x1b[1;32m {" <> show method <> ", " <> resourcePath <> "}\t\x1b[1;33m→\x1b[1;32m " <> scope <> "." <> opid <> "\x1b[0m"
    pure routeTbl

makeResourceOperationMapTest :: forall @schema @map aux. MakeResourceOperationMap schema map => Proxy aux /\ Proxy map /\ Array (String /\ HashMap (String /\ String) (Array String))
makeResourceOperationMapTest = Proxy /\ Proxy /\ makeResourceOperationMap (Proxy :: _ schema)

class MakeResourceOperationMap
  :: Type
  -> KRouteTable
  -> Constraint
class MakeResourceOperationMap schema map | schema -> map where
  makeResourceOperationMap :: Proxy schema -> Array (Tuple String (HashMap (String /\ String) (Array String)))

instance
  ( SchemaOperations schema ops
  , TypeEquals (Record opsR) ops
  , RowToList opsR opsL
  , MakeResourceOperationMapRL schema opsL mapL
  ) =>
  MakeResourceOperationMap schema mapL
  where
  makeResourceOperationMap schema =
    makeResourceOperationMapRL
      schema
      (Proxy :: _ opsL)

class MakeResourceOperationMapRL
  :: Type
  -> RowList Type
  -> KRouteTable
  -> Constraint
class MakeResourceOperationMapRL schema opsL map | schema opsL -> map where
  makeResourceOperationMapRL
    :: Proxy schema
    -> Proxy opsL
    -> Array
         ( Tuple
             String
             (HashMap (String /\ String) (Array String))
         )

instance
  MakeResourceOperationMapRL schema RL.Nil TList.Nil
  where
  makeResourceOperationMapRL _ _ = []

instance
  ( MakeResourceOperationMapRL schema tl mapTl
  , TypeEquals (Scope pfx ext sopsRec) sops
  , MakeScopedResourceOperationMap sopsRec scope mapHd
  , TypeEquals map (TList.Cons (TTuple.Tuple scope mapHd) mapTl)
  , Reflectable scope String
  ) =>
  MakeResourceOperationMapRL schema (RL.Cons scope sops tl) map
  where
  makeResourceOperationMapRL schema _ =
    let
      mapTl = makeResourceOperationMapRL schema (Proxy :: _ tl)
      mapHd = makeScopedResourceOperationMap (Proxy :: _ sopsRec) (Proxy :: _ scope)
    in
      Array.cons ((reflectType (Proxy :: _ scope)) /\ mapHd) mapTl

makeScopedResourceOperationMapTest :: forall @schema @scope map. MakeScopedResourceOperationMap schema scope map => Proxy map /\ HashMap (String /\ String) (Array String)
makeScopedResourceOperationMapTest = Proxy /\ makeScopedResourceOperationMap (Proxy :: _ schema) (Proxy :: _ scope)

class MakeScopedResourceOperationMap
  :: Type
  -> Symbol
  -> KScopedRouteTable
  -> Constraint
class MakeScopedResourceOperationMap ops scope map | ops scope -> map where
  makeScopedResourceOperationMap :: Proxy ops -> Proxy scope -> HashMap (String /\ String) (Array String)

instance
  ( RowToList opsR opsL
  , MakeScopedResourceOperationMapRowList opsR scope opsL map
  ) =>
  MakeScopedResourceOperationMap (Record opsR) scope map
  where
  makeScopedResourceOperationMap _ scope = HM.fromFoldable $ makeScopedResourceOperationMapRL (Proxy :: _ opsR) scope (Proxy :: _ opsL)

class MakeScopedResourceOperationMapRowList
  :: Row Type
  -> Symbol
  -> RowList Type
  -> KScopedRouteTable
  -> Constraint
class MakeScopedResourceOperationMapRowList ops scope opsL map | ops scope opsL -> map where
  makeScopedResourceOperationMapRL :: Proxy ops -> Proxy scope -> Proxy opsL -> Array (Tuple String String /\ (Array String))

instance MakeScopedResourceOperationMapRowList r scope RL.Nil TList.Nil
  where
  makeScopedResourceOperationMapRL _ _ _ = []

instance
  ( MakeScopedResourceOperationMapRowList r scope tl mapRest
  , TypeEquals (Endpoint method path infs) endpoint
  , TypeEquals methodPath (TTuple.Tuple method path)
  , TypeEquals elem (TTuple.Tuple methodPath (Scoped scope opid))
  , Reflectable method HTTPurple.Method
  , Reflectable scope String
  , Reflectable path String
  , Reflectable opid String
  ) =>
  MakeScopedResourceOperationMapRowList
    r
    scope
    (RL.Cons opid endpoint tl)
    (TList.Cons elem mapRest)
  where
  makeScopedResourceOperationMapRL ops scope _ =
    let
      rest = makeScopedResourceOperationMapRL ops scope (Proxy :: _ tl)
      methodPath = Tuple
        (show $ reflectType (Proxy :: _ method))
        (reflectType (Proxy :: _ path))
      elem = Tuple
        methodPath
        [ reflectType (Proxy :: _ scope)
        , reflectType (Proxy :: _ opid)
        ]
    in
      Array.cons elem rest

