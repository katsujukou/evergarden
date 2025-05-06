module Type.Data.List where

import Prim.Boolean (False, True)

data List :: forall k. k -> Type
data List k 

foreign import data Nil :: forall k. List k 

foreign import data Cons :: forall k. k -> List k -> List k 

infixr 6 type Cons as :

class Null :: forall k. List k -> Boolean -> Constraint 
class Null l b | l -> b 

instance Null Nil True
instance Null (Cons hd tl) False