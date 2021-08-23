module Data.Test.Statements exposing (tests)

import Data.Example exposing (Example)
import Dict exposing (Dict)
import Leaf exposing (Value(..))


tests : Dict String Example
tests =
    [ ( "LetExample"
      , { code = """let hello = "Hello ";
let world = "World";
hello.append world"""
        , result = StringVal "Hello World"
        }
      )
    , ( "MutExample"
      , { code = """let hello = "Hello ";
mut out = "World";
out = hello.append out;
out"""
        , result = StringVal "Hello World"
        }
      )
    , ( "Comments"
      , { code = """//This is a single line comment
let hello =
  //Comments can be in front of any statement or expression
  "Hello ";
/* This a multi line comment
/* It may even have nested comment */*/
hello.append "World" """
        , result = StringVal "Hello World"
        }
      )
    ]
        |> Dict.fromList
