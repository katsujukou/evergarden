module Evergarden.Schema.Toplevel where


import Prim.TypeError (Text)
import Type.Data.Maybe (class FromJustOrFail)
import Type.Extra.Row (class GetAt)

class SchemaResource :: Type -> Type -> Constraint
class SchemaResource schema resource | schema -> resource 

instance
  ( GetAt scmR "resources" mbresource 
  , FromJustOrFail 
      ( Text "You must specify resource type at the toplevel field of the schema." ) 
      mbresource 
      resource
  ) => SchemaResource (Record scmR) resource

class SchemaOperations :: Type -> Type -> Constraint 
class SchemaOperations schema operations | schema -> operations 

instance 
  ( GetAt scmR "operations" mboperations 
  , FromJustOrFail 
    ( Text "You must specify operations at the toplevel field of the schema." )
    mboperations 
    operations
  ) => SchemaOperations (Record scmR) operations
