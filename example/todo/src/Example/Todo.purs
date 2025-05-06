module Example.Todo where

import Prelude hiding ((/))

import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Evergarden.Codec.Argonaut (jsonEngine, JsonDecoder, JsonEncoder)
import Evergarden.Schema (Extensions, Scope)
import Evergarden.Schema.Endpoint (GET, NoInput, POST)
import Evergarden.Schema.Routing (registerRoutes)
import Evergarden.Schema.Routing.ConstructorTag (getConstructorTag)
import HTTPurple (notFound)
import HTTPurple as HTTPurple
import HTTPurple.Body (RequestBody)
import Routing.Duplex (RouteDuplex', boolean, int, optional, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Type.Data.Maybe (Nothing)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type ItemId = Int

itemId :: CA.JsonCodec ItemId 
itemId = CA.int 

type DateTime = String

datetime :: CA.JsonCodec DateTime
datetime = CA.string

type TodoItem = { id :: ItemId, label :: String, dueDate :: DateTime }

todoItem :: CA.JsonCodec TodoItem 
todoItem = CA.object "TodoItem" $ 
  CAR.record 
      { id: itemId
      , label: CA.string
      , dueDate: datetime
      }

type GetTodoInput = NoInput

type GetTodoOutput = { todo :: TodoItem }

getTodoOutput :: CA.JsonCodec GetTodoOutput
getTodoOutput = CA.object "GetTodoOutput" $ CAR.record { todo: todoItem }

type CreateTodoInput = { label :: String, dueDate :: DateTime }
createTodoInput :: CA.JsonCodec CreateTodoInput
createTodoInput = CA.object "CreateTodoInput" $ 
  CAR.record { label: CA.string, dueDate: datetime }

type CreateTodoOutput = { itemId :: ItemId }

createTodoOutput :: CA.JsonCodec CreateTodoOutput
createTodoOutput = CA.object "CreateTodoOutput" $ 
  CAR.record { itemId: itemId }

data Resource
  = Todo ItemId
  | Todos { sort :: Maybe String }
  | Hello
  | Whoa Int String Boolean

derive instance genericResource :: Generic Resource _
instance Show Resource where
  show = genericShow

resources :: RouteDuplex' Resource
resources = root $ sum
  { "Todos": "todos" ? { sort: optional <<< string }
  , "Todo": "todos" / int segment
  , "Hello": "hello" / noArgs
  , "Whoa": "whoa" / int segment / segment / boolean segment
  }

type MySchema =
  { resources :: Resource -- The resource type manipulated within our APi.
  , operations :: --        List of operations offered by our API.
      { publicApi :: --     Namespaced schema
          Scope "/public" () --             Scoped operation group. path prefix and scoped extensions are specified.
            { getTodo :: -- operationId
                GET "Todo" -- endpoint
                  { input :: GetTodoInput
                  , output :: GetTodoOutput
                  }
            , listTodos ::
                GET "Todos"
                  { output ::
                      { items :: Array TodoItem }
                  }
            , createTodo ::
                POST "Todos"
                  { input :: CreateTodoInput
                  , output :: CreateTodoOutput
                  }
            }
      -- , adminApi ::
      --     Scope "/admin" (REQUIRE_AUTH ())
      --       { deleteTodos :: 
      --           DELETE "Todos"
      --             { output :: { msg :: String }
      --             }
      --       }
      }
  , extensions :: Extensions () -- global extensions.
  }

r :: Aff (HashMap _ _)
r = registerRoutes (Proxy :: _ MySchema) (Proxy :: _ Json)
  { resources
  , jsonEngine 
  , operations:
      { publicApi:
          { handlers:
              { createTodo:
                  { decoder: decoder createTodoInput
                  , handler: \_ input -> do
                      Console.logShow input
                      pure { itemId: 42 }
                  , encoder: encoder createTodoOutput
                  }

              , getTodo:
                  { handler: \id -> do
                      Console.logShow id
                      pure
                        { todo:
                            { id
                            , label: "hoge"
                            , dueDate: (unsafeCoerce 0) :: DateTime
                            }
                        }
                  , encoder: encoder getTodoOutput
                  }
              , listTodos:
                  { handler: \inps -> do
                      let _ = spy "inps" inps
                      pure { items: [] }
                  , encoder: encoder $ CA.object "ListTodosOutput" $ 
                      CAR.record { items: CA.array todoItem } 
                  }
              }
          }
      }
  }
  where
    decoder :: forall a. CA.JsonCodec a -> JsonDecoder a
    decoder codec = lmap CA.printJsonDecodeError <<< CA.decode codec 

    encoder :: forall a. CA.JsonCodec a -> JsonEncoder a
    encoder codec = CA.encode codec 

t :: HTTPurple.Request Resource -> Aff HTTPurple.Response
t req = do
  hm <- r 
  case HashMap.lookup "publicApi" hm  of 
    Nothing -> notFound
    Just scopedmap -> do
      case HashMap.lookup ((show req.method) /\ spy "ctag" (getConstructorTag req.route)) scopedmap of 
        Nothing -> notFound
        Just router -> router req 

main :: Effect Unit
main = launchAff_ do
  string <- liftEffect $ Ref.new $ Just "{ \"label\": \"foo\", \"dueDate\": \"2025-05-06T11:45:14.000Z\" }"
  let 
    body :: RequestBody
    body = unsafeCoerce { string }
  resp <- t (unsafeCoerce { method: HTTPurple.Get, route: Todo 42, body })
  Console.logShow resp.status 