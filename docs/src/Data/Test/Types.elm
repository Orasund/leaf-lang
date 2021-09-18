module Data.Test.Types exposing (objectGetterString, tests, testsInElm)

import Data.Example exposing (Example)
import Dict exposing (Dict)
import Expect
import Leaf exposing (Value(..))
import Leaf.Core as Core
import Test exposing (..)


tests : Dict String Example
tests =
    [ ( "DynamicallyTyped"
      , { code = """mut out = 42;
set out = "Hello World";
out"""
        , result = StringVal "Hello World"
        }
      )
    , ( "NullValue"
      , { code = """isNull null"""
        , result = BoolVal True
        }
      )
    , ( "NullableValue"
      , { code = """let value = "edit me";
if (isNull value)
  "You can't append null"
  ("Hello " .append value)
"""
        , result = StringVal "Hello edit me"
        }
      )
    , ( "TrueValue"
      , { code = """isBool true"""
        , result = BoolVal True
        }
      )
    , ( "FalseValue"
      , { code = """isBool false"""
        , result = BoolVal True
        }
      )
    , ( "StringValue"
      , { code = """mut out = "Hello World";
set out = 
  if (out .isString)
    out
    null;
let error =
  if (out .isNull)
    "Error: String expected"
    null;
out"""
        , result = StringVal "Hello World"
        }
      )
    , ( "IntValue"
      , { code = """42 .isInt"""
        , result = BoolVal True
        }
      )
    , ( "FloatValue"
      , { code = """42.0 .isFloat"""
        , result = BoolVal True
        }
      )
    , ( "ListValue"
      , { code = """[ [] .isList,
  [ 1,
    "Hello World",
    isNull null,
    ( let isAwesome = true;
      if isAwesome 42.0 42
    )
  ]
]"""
        , result = ListVal [ BoolVal True, ListVal [ IntVal 1, StringVal "Hello World", BoolVal True, FloatVal 42 ] ]
        }
      )
    , ( "ObjValue"
      , { code = """{ helloWorld : null,
  favoriteNumber : 42
}"""
        , result = ObjectVal (Dict.fromList [ ( "favoriteNumber", IntVal 42 ), ( "helloWorld", NullVal ) ])
        }
      )
    , ( "EmptyObj"
      , { code = """{} .isObject"""
        , result = BoolVal True
        }
      )
    , ( "FunctionValue"
      , { code = """(fun hello world -> hello .append world) "Hello" "World"
  .isFunction"""
        , result = BoolVal True
        }
      )
    , ( "MutFunValue"
      , { code = """mut out = fun a b -> append a b;
set out = "Hello World";
out"""
        , result = StringVal "Hello World"
        }
      )
    , ( "CurryFun"
      , { code = """mut appendTo = fun a b -> append a b;
(appendTo "World") .isFunction"""
        , result = BoolVal True
        }
      )
    , ( "ExtensionValue"
      , { code = """append .isExtension"""
        , result = BoolVal True
        }
      )
    ]
        |> Dict.fromList


objectGetterString : String
objectGetterString =
    """let
    objectPackage =
      [ (\\string obj ->
          obj
          |> Dict.get string
          |> Maybe.withDefault NullVal 
        )
          |> Leaf.binaryFun (Leaf.typed Leaf.asString)
              (Leaf.typed Leaf.asObject)
          |> Leaf.field "get"
      ]
          |> Dict.fromList

    context =
        Core.package
          |> Leaf.addPackage "Object" objectPackage
in
"{ string : \\"Hello World\\"} .Object::get \\"string\\""
    |> Leaf.run context"""


testsInElm : Test
testsInElm =
    Test.describe "Types Elm Test"
        [ Test.test "Object Getter" <|
            \_ ->
                let
                    objectPackage =
                        [ (\string obj ->
                            obj
                                |> Dict.get string
                                |> Maybe.withDefault NullVal
                          )
                            |> Leaf.binaryFun (Leaf.typed Leaf.asString)
                                (Leaf.typed Leaf.asObject)
                            |> Leaf.field "get"
                        ]
                            |> Dict.fromList

                    context =
                        Core.package
                            |> Leaf.addPackage "Object" objectPackage
                in
                "{ string : \"Hello World\"} .Object::get \"string\""
                    |> Leaf.run context
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok <|
                            StringVal "Hello World"
                        )
        ]
