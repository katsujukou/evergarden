module Type.Data.Tuple where

data KTuple :: forall k1 k2. k1 -> k2 -> Type
data KTuple a b 

infixr 6 type KTuple as * 

foreign import data Tuple :: forall k1 k2. k1 -> k2 -> KTuple k1 k2

infixr 6 type Tuple as ./\

class Fst :: forall k1 k2. KTuple k1 k2 -> k1 -> Constraint
class Fst ab a | ab -> a 

instance Fst (Tuple a b) a 

class Snd :: forall k1 k2. KTuple k1 k2 -> k2 -> Constraint
class Snd ab b | ab -> b

instance Snd (Tuple a b) b 
