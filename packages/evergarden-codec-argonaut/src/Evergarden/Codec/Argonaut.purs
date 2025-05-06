module Evergarden.Codec.Argonaut where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Evergarden.Json.Engine (JsonEngine(..))
import Evergarden.Json.Engine as Evergarden.Json

type JsonDecoder a = Evergarden.Json.JsonDecoder Json a 

type JsonEncoder a = Evergarden.Json.JsonEncoder a Json

jsonEngine :: JsonEngine Json
jsonEngine = JsonEngine
  { deserialize: jsonParser 
  , serialize: Json.stringify
  }