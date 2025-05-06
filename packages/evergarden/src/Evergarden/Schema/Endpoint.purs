module Evergarden.Schema.Endpoint where

import Evergarden.Schema.Method (Delete, Get, Method, Options, Patch, Post, Put)
import Prim.Boolean (False, True)

foreign import data Endpoint :: Method -> Symbol -> Type -> Type

type GET resource spec = Endpoint Get resource spec

type POST resource spec = Endpoint Post resource spec

type PUT resource spec = Endpoint Put resource spec

type PATCH resource spec = Endpoint Patch resource spec

type DELETE resource spec = Endpoint Delete resource spec

type OPTIONS resource spec = Endpoint Options resource spec

foreign import data NoInput :: Type

class IsNoInput :: Type -> Boolean -> Constraint
class IsNoInput t b | t -> b 

instance IsNoInput NoInput True
else instance IsNoInput _t False

foreign import data Scoped :: forall k. Symbol -> k -> Type 
