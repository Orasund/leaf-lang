module Introduction exposing (helloWorldTest, suite)

import Data.Example exposing (Example)
import Data.Test.Introduction as T
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Leaf exposing (Field, Value(..))
import Leaf.Core as Core
import Test exposing (..)


evaluateTest : Example -> Expectation
evaluateTest example =
    example.code
        |> Leaf.eval Dict.empty
        |> Result.map Tuple.first
        |> Expect.equal (Ok example.result)


helloWorldTest : Test
helloWorldTest =
    T.helloWorldTest


suite : Test
suite =
    T.tests
        |> Dict.toList
        |> List.map
            (\( name, example ) ->
                Test.test name <|
                    \_ ->
                        evaluateTest example
            )
        |> Test.describe "Test Suite"
