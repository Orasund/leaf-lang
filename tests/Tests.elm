module Tests exposing (..)

import Ast exposing (Exp(..), Number(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Semantics
import Syntax
import Test exposing (Test)
import Util exposing (Value(..))


expectError : String -> Expectation
expectError string =
    case
        string
            |> Syntax.parse
    of
        Ok ok ->
            ok
                |> Semantics.eval
                |> Expect.err

        Err err ->
            Expect.fail err


expectSuccess : Value -> String -> Expectation
expectSuccess value string =
    string
        |> Syntax.parse
        |> Result.andThen Semantics.eval
        |> Expect.equal (Ok value)


exp : Test
exp =
    Test.describe "Exp Syntax"
        [ Test.test "Constant Exp" <|
            \_ ->
                "42"
                    |> expectSuccess (NumberVal (IntNum 42))
        , Test.test "Variable Exp" <|
            \_ ->
                "a"
                    |> expectError
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
