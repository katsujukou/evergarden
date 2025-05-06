module Evergarden.Schema.Routing.ConstructorTag where

import Prelude

import Data.Generic.Rep (class Generic, Constructor, Sum(..), from)
import Data.Reflectable (class Reflectable, reflectType)
import Type.Proxy (Proxy(..))

-- | The "ConstructorTag" type class represents those types 
--   whose inhabits allow to extract "the name of the constructor"
--   which is used to create that value in runtime.
class ConstructorTag a where
  getConstructorTag :: a -> String

instance constructorTag :: 
  ( Generic a rep 
  , ConstructorRepTag rep
  ) => ConstructorTag a 
  where
    getConstructorTag = from >>> getConstructorRepTag 

class ConstructorRepTag rep where
  getConstructorRepTag :: rep -> String

instance constructorConstructorRepTag ::
  ( Reflectable cstr String 
  ) => ConstructorRepTag (Constructor cstr _1)
  where
    getConstructorRepTag _ = reflectType (Proxy :: _ cstr)

instance sumConstructorRepTag ::
  ( ConstructorRepTag l
  , ConstructorRepTag r
  ) => ConstructorRepTag (Sum l r) 
  where
  getConstructorRepTag = case _ of 
    Inl l -> getConstructorRepTag l 
    Inr r -> getConstructorRepTag r