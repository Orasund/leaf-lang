module VerifyExamples.Leaf.ModuleDoc1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Leaf exposing (..)
import Leaf.Core as Core
import Dict exposing (Dict)
import Leaf exposing (Value(..))
import Leaf exposing (Value(..))



context : Dict String Field
context =
    [ StringVal "World" |> Leaf.field "name"
    , (\s2 s1 -> StringVal (s1 ++ s2))
        |> Leaf.binaryFun (Leaf.typed Leaf.asString)
            (Leaf.typed Leaf.asString)
        |> Leaf.field "append"
    ]
        |> Dict.fromList
        |> Leaf.addExposed Core.package



spec1 : Test.Test
spec1 =
    Test.test "Module VerifyExamples: \n\n    \"\\\"Hello World\\\"\"\n        |> Leaf.run Dict.empty\n    --> Ok (StringVal \"Hello World\",Dict.empty)" <|
        \() ->
            Expect.equal
                (
                "\"Hello World\""
                    |> Leaf.run Dict.empty
                )
                (
                Ok (StringVal "Hello World",Dict.empty)
                )