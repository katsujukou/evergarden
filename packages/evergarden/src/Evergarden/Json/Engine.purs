module Evergarden.Json.Engine where

import Data.Either (Either)

type JsonDecoder json input = json -> Either String input 

type JsonEncoder output json = output -> json 

newtype JsonEngine json = JsonEngine
  { deserialize :: String -> Either String json 
  , serialize :: json -> String
  }

