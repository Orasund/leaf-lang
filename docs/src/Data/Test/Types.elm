module Data.Test.Types exposing (tests)

import Data.Example exposing (Example)
import Dict exposing (Dict)
import Leaf exposing (Value(..))


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
  ("Hello ".append value)
"""
        , result = BoolVal True
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
  if (out.isString)
    out
    null;
let error =
  if (out.isNull)
    "Error: String expected"
    null;
out"""
        , result = BoolVal True
        }
      )
    , ( "IntValue"
      , { code = """//Note that we need to use brackets here
(42).isInt"""
        , result = BoolVal True
        }
      )
    , ( "FloatValue"
      , { code = """42.0.isFloat"""
        , result = BoolVal True
        }
      )
    , ( "ListValue"
      , { code = """[ [].isList,
  [ 1,
    "Hello World",
    isNull null,
    ( let isAwesome = true;
      if isAwesome 42.0 42
    )
  ]
]"""
        , result = BoolVal True
        }
      )
    , ( "ObjValue"
      , { code = """{ helloWorld : null,
  favoriteNumber : 42
}"""
        , result = BoolVal True
        }
      )
    , ( "EmptyObj"
      , { code = """{}.isObject"""
        , result = BoolVal True
        }
      )
    , ( "FunctionValue"
      , { code = """(fun hello world -> hello.append world) "Hello" \"World\"
  .isFunction"""
        , result = BoolVal True
        }
      )
    , ( "MutFunValue"
      , { code = """mut a = fun string -> "Hello".append string;
let a = "World";
a"""
        , result = BoolVal True
        }
      )
    , ( "CurryFun"
      , { code = """mut appendTo = fun a b -> append a b;
(appendTo "World").isFunction"""
        , result = BoolVal True
        }
      )
    , ( "ExtensionValue"
      , { code = """append.isExtension"""
        , result = BoolVal True
        }
      )
    ]
        |> Dict.fromList
