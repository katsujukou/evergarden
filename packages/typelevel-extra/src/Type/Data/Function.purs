module Type.Data.Function where

import Data.Tuple (Tuple)

class Curry :: Type -> Type -> Constraint
class Curry u c | u -> c

instance
  ( Curry (Function b c) bc 
  ) => Curry (Function (Tuple a b) c) (Function a bc) 
else instance Curry f f
