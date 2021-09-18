module Types exposing (testsInElm)

import Data.Example exposing (Example)
import Data.Test as Test
import Data.Test.Types as T
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


testsInElm : Test
testsInElm =
    T.testsInElm