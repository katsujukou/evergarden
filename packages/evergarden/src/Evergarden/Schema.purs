module Evergarden.Schema where

data Schema :: Type -> Type -> Type
data Schema resource operations 

data Scope :: Symbol -> Row Type -> Type -> Type
data Scope prefix exts scm

data Extensions :: Row Type -> Type 
data Extensions ext
