module Introduction exposing (helloWorldTest, suite)

import Data.Example exposing (Example)
import Data.Test as Test
import Data.Test.Introduction as T
import Dict
import Expect exposing (Expectation)
import Leaf exposing (Value(..))
import Test exposing (..)


evaluateTest : Example -> Expectation
evaluateTest example =
    example.code
        |> Leaf.run Test.context
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
