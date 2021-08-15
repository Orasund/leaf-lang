module Syntax exposing (parse)

import Ast exposing (BuildIn, Closure, Exp(..), Number(..), Statement(..))
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Set
import Util


parseVariable : Parser String
parseVariable =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "let", "mut", "null", "true", "false" ]
        }


parseBool : Parser Bool
parseBool =
    Parser.oneOf
        [ Parser.succeed (always True)
            |= Parser.keyword "true"
        , Parser.succeed (always False)
            |= Parser.keyword "false"
        ]


parseNumber : Parser Number
parseNumber =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "-"
            |= Parser.number
                { int = Just (negate >> IntNum)
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                , float = Just (negate >> FloatNum)
                }
        , Parser.number
            { int = Just IntNum
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Just FloatNum
            }
        ]


parseList : Parser (List Exp)
parseList =
    Parser.oneOf
        [ Parser.succeed (always [])
            |= Parser.keyword "[]"
        , Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = parseExp
            , trailing = Forbidden
            }
        ]


parseObjectField : Parser ( String, Exp )
parseObjectField =
    Parser.succeed Tuple.pair
        |= parseVariable
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= parseExp


parseObject : Parser (Dict String Exp)
parseObject =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = Parser.spaces
        , item = parseObjectField
        , trailing = Forbidden
        }
        |> Parser.map Dict.fromList



{--|An Expression starting with "("

* unit value: "()"
* variable "(a)"
* function: "(a,b,c) -> exp"

--}


roundBracketExp : Parser Exp
roundBracketExp =
    let
        {--| build a function with multiple arguments --}
        buildFunction : String -> List String -> Exp -> Exp
        buildFunction last list exp =
            case list |> List.reverse of
                [] ->
                    FunctionExp (Just last) exp

                head :: tail ->
                    tail
                        |> List.foldl (\a -> FunctionExp (Just a))
                            (FunctionExp (Just head) exp)
                        |> FunctionExp (Just last)

        {--| parses the function arguments "(b,c)"
        --}
        functionHelp : List String -> Parser (Step (List String) (List String))
        functionHelp revList =
            Parser.oneOf
                [ Parser.succeed (\a -> Loop (a :: revList))
                    |= parseVariable
                    |. Parser.spaces
                    |. Parser.symbol ","
                    |. Parser.spaces
                , Parser.succeed (\a -> Done (a :: revList |> List.reverse))
                    |= parseVariable
                    |. Parser.spaces
                    |. Parser.symbol ")"
                ]

        {--| decides if "(a)" is part of a function "(a) -> exp" or a variable
        --}
        functionOrVariable : String -> Parser Exp
        functionOrVariable a =
            Parser.oneOf
                [ --"(a)"
                  Parser.succeed identity
                    |. Parser.symbol ")"
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ --"(a) -> exp"
                          Parser.succeed (FunctionExp (Just a))
                            |. Parser.keyword "->"
                            |. Parser.spaces
                            |= parseExp
                        , --else
                          Parser.succeed (Variable a)
                        ]
                , --"(a,b,c) -> exp"
                  Parser.succeed (buildFunction a)
                    |. Parser.symbol ","
                    |. Parser.spaces
                    |= Parser.loop [ a ] functionHelp
                    |. Parser.spaces
                    |. Parser.keyword "->"
                    |. Parser.spaces
                    |= parseExp
                ]
    in
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= Parser.oneOf
            [ -- "()"
              Parser.succeed identity
                |. Parser.symbol ")"
                |. Parser.spaces
                |= Parser.oneOf
                    [ --"() -> exp"
                      Parser.succeed (FunctionExp Nothing)
                        |. Parser.keyword "->"
                        |. Parser.spaces
                        |= parseExp
                    , --else
                      Parser.succeed NullExp
                    ]
            , --"(a"
              (Parser.succeed identity
                |= parseVariable
                |. Parser.spaces
              )
                |> Parser.andThen functionOrVariable
            , --"(exp)"
              Parser.succeed identity
                |= parseExp
                |. Parser.spaces
                |. Parser.symbol ")"
                |> Parser.andThen
                    (\exp1 ->
                        Parser.oneOf
                            [ --"(exp) exp"
                              Parser.succeed (\exp2 -> Apply exp2 exp1)
                                |. Parser.spaces
                                |= parseExp
                            , --else
                              Parser.succeed exp1
                            ]
                    )
            ]


parseBuildIn : Parser BuildIn
parseBuildIn =
    Parser.problem "implement buildin functions"


parseExp : Parser Exp
parseExp =
    Parser.oneOf
        [ parseBuildIn |> Parser.map BuildInFun
        , parseVariable |> Parser.map Variable
        , Parser.lazy (\_ -> roundBracketExp)
        , Parser.keyword "null" |> Parser.map (always NullExp)
        , parseBool |> Parser.map BoolExp
        , parseNumber |> Parser.map NumberExp
        , Parser.lazy (\_ -> parseList) |> Parser.map ListExp
        , Parser.backtrackable <|
            Parser.succeed ClosureExp
                |. Parser.symbol "{"
                |. Parser.spaces
                |= Parser.lazy (\_ -> parseClosure)
                |. Parser.spaces
                |. Parser.symbol "}"
        , Parser.lazy (\_ -> parseObject) |> Parser.map ObjectExp
        ]
        |> Parser.andThen
            (\exp1 ->
                --"exp"
                Parser.oneOf
                    [ --"exp exp"
                      Parser.succeed (\exp2 -> Apply exp2 exp1)
                        |. Parser.spaces
                        |= Parser.lazy (\_ -> parseExp)
                    , --else
                      Parser.succeed exp1
                    ]
            )


parseStatement : Parser Statement
parseStatement =
    Parser.oneOf
        [ Parser.succeed Let
            |. Parser.keyword "let"
            |. Parser.spaces
            |= parseVariable
            |. Parser.spaces
            |. Parser.symbol "="
            |. Parser.spaces
            |= parseExp
        , Parser.succeed Mut
            |. Parser.keyword "mut"
            |. Parser.spaces
            |= parseVariable
            |. Parser.spaces
            |. Parser.symbol "="
            |. Parser.spaces
            |= parseExp
        , Parser.succeed Set
            |= parseVariable
            |. Parser.spaces
            |. Parser.symbol "="
            |. Parser.spaces
            |= parseExp
        ]


parseClosure : Parser Closure
parseClosure =
    let
        statementsHelp : List Statement -> Parser (Step (List Statement) Closure)
        statementsHelp revStmts =
            Parser.oneOf
                [ Parser.backtrackable <|
                    Parser.succeed (\stmt -> Loop (stmt :: revStmts))
                        |= parseStatement
                        |. Parser.spaces
                        |. Parser.symbol ";"
                        |. Parser.spaces
                , parseExp
                    |> Parser.map
                        (\exp ->
                            Done
                                { statements = List.reverse revStmts
                                , return = exp
                                }
                        )
                ]
    in
    Parser.loop [] statementsHelp


parse : String -> Result String Closure
parse =
    Parser.run
        (Parser.succeed identity
            |= parseClosure
            |. Parser.spaces
            |. Parser.end
        )
        >> Result.mapError Util.deadEndsToString
