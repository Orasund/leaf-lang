module Tests exposing (..)

import Dict
import El.Internal.Semantics as Semantics
import El.Internal.Syntax as Syntax
import El.Language as Language exposing (Exp(..), Number(..), Value(..))
import El.Util
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Leaf.Core as Core
import Test exposing (Test)


expectError : String -> Expectation
expectError string =
    case
        string
            |> Syntax.parse
    of
        Ok ok ->
            ok
                |> Semantics.eval Core.package
                |> Expect.err

        Err err ->
            Expect.fail err


expectSuccess : Value -> String -> Expectation
expectSuccess value string =
    string
        |> Syntax.parse
        |> Result.andThen (Semantics.eval Core.package)
        |> Expect.equal (Ok value)


exp : Test
exp =
    Test.describe "Exp Syntax"
        [ Test.test "Constant Exp" <|
            \_ ->
                "42"
                    |> expectSuccess (NumberVal (IntNum 42))
        , Test.test "String Exp" <|
            \_ ->
                "\"Hello World\""
                    |> expectSuccess (StringVal "Hello World")
        , Test.test "Variable Exp" <|
            \_ ->
                "a"
                    |> expectError
        , Test.test "Types" <|
            \_ ->
                "[-1,42.,{a:42}]"
                    |> expectSuccess
                        (ListVal
                            [ NumberVal (IntNum -1)
                            , NumberVal (FloatNum 42.0)
                            , ObjectVal
                                (Dict.fromList
                                    [ ( "a", NumberVal (IntNum 42) ) ]
                                )
                            ]
                        )
        , Test.test "Basic Function" <|
            \_ ->
                "(() -> 42) null"
                    |> expectSuccess (NumberVal (IntNum 42))
        , Test.test "Uniary Function" <|
            \_ ->
                "((a) -> 42) null"
                    |> expectSuccess (NumberVal (IntNum 42))
        ]


closureExp : Test
closureExp =
    Test.describe "Closure Exp"
        [ Test.test "trivial closure" <|
            \_ ->
                "let a = 42;{a}"
                    |> expectSuccess (NumberVal (IntNum 42))
        , Test.test "overwriting variable in closure" <|
            \_ ->
                "mut a = 0;{a = 42; a}"
                    |> expectError
        ]


statement : Test
statement =
    Test.describe "let Statement"
        [ Test.test "Valid let" <|
            \_ ->
                "let a = 42;a"
                    |> expectSuccess (NumberVal (IntNum 42))
        , Test.test "overwriding let variable" <|
            \_ ->
                "let a = 0; a = 42; a"
                    |> expectError
        , Test.test "recursive definition" <|
            \_ ->
                "let a = a; a"
                    |> expectError
        , Test.test "mut variable" <|
            \_ ->
                "mut a = 0; a = 42; a"
                    |> expectSuccess (NumberVal (IntNum 42))
        ]


coreLib : Test
coreLib =
    Test.describe "Core Libary"
        [ Test.test "Eq" <|
            \_ ->
                "equal 42 0"
                    |> expectSuccess (BoolVal False)
        , Test.test "Application" <|
            \_ ->
                "equal (equal 42 0) false"
                    |> expectSuccess (BoolVal True)
        ]
