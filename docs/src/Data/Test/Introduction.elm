module Data.Test.Introduction exposing (helloWorldString, helloWorldTest, tests)

import Data.Example exposing (Example)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Leaf exposing (Value(..))
import Test exposing (..)


tests : Dict String Example
tests =
    [ ( "HelloWorld"
      , { code = "\"Hello World\""
        , result = StringVal "Hello World"
        }
      )
    ]
        |> Dict.fromList


helloWorldString : String
helloWorldString =
    """
    "\\"Hello World\\""
        |> Leaf.run Dict.empty
"""


helloWorldTest : Test
helloWorldTest =
    Test.describe "Introduction Elm Test"
        [ Test.test "Hello World in Elm" <|
            \_ ->
                "\"Hello World\""
                    |> Leaf.run Dict.empty
                    |> Expect.equal
                        (Ok <|
                            ( StringVal "Hello World", Dict.empty )
                        )
        , Test.test "Context-sensitive in Elm" <|
            \_ ->
                "\"Hello \".append name"
                    |> Leaf.run Dict.empty
                    |> Expect.equal
                        (Ok <|
                            ( StringVal "Hello World", Dict.empty )
                        )
        ]
