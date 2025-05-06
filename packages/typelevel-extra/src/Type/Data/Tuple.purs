module Type.Data.Tuple where

import Prelude

data KTuple :: forall k1 k2. k1 -> k2 -> Type
data KTuple a b 

foreign import data Tuple :: forall k1 k2. k1 -> k2 -> KTuple k1 k2

class Fst :: forall k1 k2. KTuple k1 k2 -> k1 -> Constraint
class Fst ab a | ab -> a 

instance Fst (Tuple a b) a 

class Snd :: forall k1 k2. KTuple k1 k2 -> k2 -> Constraint
class Snd ab b | ab -> b

instance Snd (Tuple a b) b 
