module Type.Data.Maybe where

import Prim.Boolean (False, True)
import Prim.TypeError (class Fail, Doc)

data Maybe :: forall k. k -> Type 
data Maybe k

foreign import data Nothing :: forall k. Maybe k 

foreign import data Just :: forall k. k -> Maybe k

class FromMaybe :: forall k. k -> Maybe k -> k -> Constraint
class FromMaybe default maybe o | default maybe -> o

instance FromMaybe default Nothing default 
instance FromMaybe _1 (Just k) k 

class FromJustOrFail :: forall k. Doc -> Maybe k -> k -> Constraint
class FromJustOrFail err mbk k | err mbk -> k 

instance
  ( Fail err 
  ) => FromJustOrFail err Nothing k

instance FromJustOrFail err (Just k) k

class IsNothing :: forall k. Maybe k -> Boolean -> Constraint
class IsNothing mb b | mb -> b 

instance IsNothing Nothing True 
instance IsNothing (Just k) False