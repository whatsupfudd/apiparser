module Generators.Base where

import Data.Text ( Text )

data PathComponent = 
  Label Text
  | Variable Text
  | Wildcard

data HttpVerb =
  Get
  | Post
  | Put
  | Delete
  | Patch
  | Options
  | Head
  | Trace

data Header = Header {
  label :: Text
  , required :: Bool
  , format :: Text
  }


data Record = Record {
  label :: Text
  , required :: Bool
  , type_ :: DataType
}  


data DataType =
  String
  | Number
  | Integer
  | Boolean
  | Array
  | Object [ Record ]
  | File
  | Null


data Parameter = Parameter {
  label :: Text
  , required :: Bool
  , type_ :: DataType
  }

data Response = Response {
  status :: Int
  , type_ :: DataType
  }

data Operation = Operation {
  method :: HttpVerb
  , headers :: [ Header ]
  , parameters :: [ Parameter ]
  , response :: Response
  }

data Route = Route { 
    path :: [ PathComponent ]
    , operations :: [ Operation ]
  }

data ClientSpec = ClientSpec {
  routes :: [ Route ]
  }

