module Evergarden.Schema.Method where

import Data.Reflectable (class Reflectable)
import HTTPurple as HTTPurple

data Method

foreign import data Get :: Method
foreign import data Post :: Method
foreign import data Put :: Method 
foreign import data Patch :: Method
foreign import data Delete :: Method 
foreign import data Options :: Method 

instance Reflectable Get HTTPurple.Method where
  reflectType _ = HTTPurple.Get

instance Reflectable Post HTTPurple.Method where
  reflectType _ = HTTPurple.Post

instance Reflectable Put HTTPurple.Method where
  reflectType _ = HTTPurple.Put

instance Reflectable Patch HTTPurple.Method where
  reflectType _ = HTTPurple.Patch

instance Reflectable Delete HTTPurple.Method where
  reflectType _ = HTTPurple.Delete

instance Reflectable Options HTTPurple.Method where
  reflectType _ = HTTPurple.Options
