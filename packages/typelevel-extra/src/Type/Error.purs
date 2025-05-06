module Type.Error where

import Prelude

import Prim.TypeError (Above, Beside, Doc, Text)

infixl 6 type Beside as ++
infixl 5 type Above as |>

class FailWhen :: Boolean -> Doc -> Constraint
class FailWhen b doc 

class Describable :: forall k t. k -> t -> Doc -> Constraint
class Describable k typ doc | k typ -> doc

instance Describable Symbol s (Text s)