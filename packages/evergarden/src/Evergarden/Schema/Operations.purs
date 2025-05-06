-- | This module defines some type-level machineries for
--   manipulating operations listed in the schema.
module Evergarden.Schema.Operations where

import Evergarden.Schema (Scope)
import Evergarden.Schema.Toplevel (class SchemaOperations)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (Quote, Text)
import Type.Data.List as TList
import Type.Data.Maybe (class FromJustOrFail)
import Type.Equality (class TypeEquals)
import Type.Error (type (++), type (|>))
import Type.Extra.Row (class GetAt)
import Type.Proxy (Proxy(..))

listScopesTest :: forall @schema scopes. ListScopes schema scopes => Proxy scopes
listScopesTest = Proxy

class ListScopes :: Type -> TList.List Symbol -> Constraint
class ListScopes schema scopes | schema -> scopes

instance
  ( SchemaOperations schema ops
  , TypeEquals (Record opsR) ops
  , RowToList opsR opsL
  , ListScopesRL opsL scopes
  ) =>
  ListScopes schema scopes

class ListScopesRL :: RowList Type -> TList.List Symbol -> Constraint
class ListScopesRL opsL scopes | opsL -> scopes

instance ListScopesRL RL.Nil TList.Nil
instance
  ( ListScopesRL tl scopesTl
  ) =>
  ListScopesRL (RL.Cons scope sops tl) (TList.Cons scope scopesTl)

class GetScopedOperations
  :: Type
  -- ^ the schema type
  -> Symbol
  -- ^ the scope
  -> Type
  -- ^ the scoped operations (`Scope prefix exts ops` assumed)  
  -> Constraint
class GetScopedOperations schema scope ops | schema scope -> ops

instance
  ( SchemaOperations scm (Record ops)
  , GetAt ops scope mbsops
  , FromJustOrFail
      (Text "Unknown scope: " ++ Quote scope)
      mbsops
      sops
  ) =>
  GetScopedOperations scm scope sops

class GetScopedOperation
  :: Type
  -- ^ schema type (Record assumed)
  -> Symbol
  -- ^ scope 
  -> Symbol
  -- ^ scoped operation id
  -> Type
  -- ^ endpoint type (Scope endpoint assumed)
  -> Constraint
class GetScopedOperation schema scope opid endpoint | schema scope opid -> endpoint

instance
  ( GetScopedOperations schema scope (Scope prefix exts sops)
  , TypeEquals sops (Record sopsR)
  , GetAt sopsR opid mbit
  , FromJustOrFail
      ( Text "Unknown operation: " ++ Text opid
          |> Text "within scope "
          ++ Text scope
      )
      mbit
      it
  ) =>
  GetScopedOperation schema scope opid it
