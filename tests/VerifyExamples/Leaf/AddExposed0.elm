module VerifyExamples.Leaf.AddExposed0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Leaf exposing (..)
import Dict







spec0 : Test.Test
spec0 =
    Test.test "#addExposed: \n\n    addExposed\n    --> Dict.union" <|
        \() ->
            Expect.equal
                (
                addExposed
                )
                (
                Dict.union
                )