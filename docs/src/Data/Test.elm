module Data.Test exposing (context, tests)

import Data.Example exposing (Example)
import Data.Test.Introduction as Introduction
import Data.Test.Statements as Statements
import Dict exposing (Dict)
import Leaf exposing (Field, Value(..))
import Leaf.Core as Core


tests : Dict String Example
tests =
    Introduction.tests
        |> Dict.union Statements.tests


context : Dict String Field
context =
    [ StringVal "World" |> Leaf.field "name"
    , (\s2 s1 -> StringVal (s1 ++ s2))
        |> Leaf.binaryFun (Leaf.typed Leaf.asString)
            (Leaf.typed Leaf.asString)
        |> Leaf.field "append"
    , (\s2 s1 -> StringVal (s1 ++ s2))
        |> Leaf.binaryFun (Leaf.typed Leaf.asString)
            (Leaf.typed Leaf.asString)
        |> Leaf.field "String::append"
    ]
        |> Dict.fromList
        |> Dict.union Core.package
