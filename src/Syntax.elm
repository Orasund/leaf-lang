module Syntax exposing (parse)

import Ast exposing (BuildIn, Closure, Exp(..), Number(..), Statement(..), Value(..))
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


parseNull : Parser ()
parseNull =
    Parser.oneOf
        [ Parser.keyword "()"
        , Parser.keyword "null"
        ]


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
        [ Parser.succeed (negate >> IntNum)
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.succeed IntNum
            |= Parser.int
        , Parser.succeed (negate >> FloatNum)
            |. Parser.symbol "-"
            |= Parser.float
        , Parser.succeed FloatNum
            |= Parser.float
        ]


parseList : Parser (List Value)
parseList =
    Parser.oneOf
        [ Parser.succeed (always [])
            |= Parser.keyword "[]"
        , Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = parseValue
            , trailing = Forbidden
            }
        ]


parseObjectField : Parser ( String, Value )
parseObjectField =
    Parser.succeed Tuple.pair
        |= parseVariable
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= parseValue


parseObject : Parser (Dict String Value)
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


parseFunctionAndNull : Parser Value
parseFunctionAndNull =
    (Parser.succeed identity
        |= Parser.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = Parser.spaces
            , item = parseVariable
            , trailing = Forbidden
            }
        |. Parser.spaces
    )
        |> Parser.andThen
            (\list ->
                case list |> List.reverse of
                    [] ->
                        Parser.oneOf
                            [ Parser.succeed (FunctionVal Nothing)
                                |. Parser.keyword "->"
                                |. Parser.spaces
                                |= parseExp
                            , Parser.succeed NullVal
                            ]

                    head :: tail ->
                        Parser.succeed
                            (\exp ->
                                tail
                                    |> List.foldl (\a e -> FunctionVal (Just a) (Constant e))
                                        (FunctionVal (Just head) exp)
                            )
                            |. Parser.keyword "->"
                            |. Parser.spaces
                            |= parseExp
            )


parseValue : Parser Value
parseValue =
    Parser.oneOf
        [ parseFunctionAndNull
        , Parser.keyword "null" |> Parser.map (always NullVal)
        , parseBool |> Parser.map BoolVal
        , parseNumber |> Parser.map NumberVal
        , Parser.lazy (\_ -> parseList) |> Parser.map ListVal
        , Parser.lazy (\_ -> parseObject) |> Parser.map ObjectVal
        ]


parseBuildIn : Parser BuildIn
parseBuildIn =
    Parser.problem "implement buildin functions"


parseExp : Parser Exp
parseExp =
    Parser.oneOf
        [ parseBuildIn |> Parser.map BuildInFun
        , Parser.succeed Apply
            |= Parser.lazy (\_ -> parseExp)
            |. Parser.spaces
            |= Parser.lazy (\_ -> parseExp)
        , Parser.succeed ClosureExp
            |. Parser.symbol "{"
            |. Parser.spaces
            |= Parser.lazy (\_ -> parseClosure)
            |. Parser.spaces
            |. Parser.symbol "}"
        , Parser.lazy (\_ -> parseValue) |> Parser.map Constant
        , parseVariable |> Parser.map Variable
        ]


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
