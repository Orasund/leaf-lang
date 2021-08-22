module Internal.Syntax exposing (parse)

import Dict exposing (Dict)
import Internal.Language exposing (Closure, Exp(..), Statement(..))
import Internal.Util as Util
import Parser exposing ((|.), (|=), Nestable(..), Parser, Step(..), Trailing(..))
import Set


parseVariable : Parser String
parseVariable =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == ':'
        , reserved = Set.fromList [ "let", "mut", "null", "true", "false" ]
        }


parseString : Parser String
parseString =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "\""
            |= Parser.variable
                { start = (/=) '"'
                , inner = (/=) '"'
                , reserved = Set.empty
                }
            |. Parser.symbol "\""
        , Parser.succeed identity
            |. Parser.symbol "'"
            |= Parser.variable
                { start = (==) '\''
                , inner = (/=) '\''
                , reserved = Set.empty
                }
            |. Parser.symbol "'"
        ]


comment : Parser ()
comment =
    Parser.oneOf
        [ Parser.lineComment "//"
        , Parser.multiComment "/*" "*/" Nestable
        ]


parseBool : Parser Bool
parseBool =
    Parser.oneOf
        [ Parser.succeed (always True)
            |= Parser.keyword "true"
        , Parser.succeed (always False)
            |= Parser.keyword "false"
        ]


parseNumber : Parser Exp
parseNumber =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "-"
            |= Parser.number
                { int = Just (negate >> IntExp)
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                , float = Just (negate >> FloatExp)
                }
        , Parser.number
            { int = Just IntExp
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Just FloatExp
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
                |> Parser.backtrackable
            , --"(exp)"
              Parser.succeed identity
                |= parseExp
                |. Parser.spaces
                |. Parser.symbol ")"
            ]


singleExp : Parser Exp
singleExp =
    Parser.oneOf
        [ Parser.succeed identity
            |. comment
            |= Parser.lazy (\_ -> singleExp)
        , parseVariable |> Parser.map Variable
        , Parser.lazy (\_ -> roundBracketExp)
        , Parser.keyword "null" |> Parser.map (always NullExp)
        , parseString |> Parser.map StringExp
        , parseBool |> Parser.map BoolExp
        , parseNumber
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


multiExp : Exp -> Parser Exp
multiExp e =
    let
        expHelp : List Exp -> Parser (Step (List Exp) (List Exp))
        expHelp revList =
            Parser.oneOf
                [ Parser.succeed (\a -> Loop (a :: revList))
                    |. Parser.spaces
                    |= singleExp
                , Parser.succeed (Done (revList |> List.reverse))
                ]
    in
    Parser.loop [] expHelp
        |> Parser.map
            (\list ->
                case list of
                    --"exp ... exp"
                    a1 :: tail ->
                        tail
                            |> List.foldl (\next f -> Apply next f)
                                (Apply a1 e)

                    --else
                    [] ->
                        e
            )


pipeExp : Exp -> Parser Exp
pipeExp e =
    let
        expHelp : List Exp -> Parser (Step (List Exp) (List Exp))
        expHelp revList =
            Parser.oneOf
                [ Parser.succeed (\a -> Loop (a :: revList))
                    |. Parser.spaces
                    |. Parser.symbol "."
                    |. Parser.spaces
                    |= (singleExp |> Parser.andThen multiExp)
                , Parser.succeed (Done (revList |> List.reverse))
                ]
    in
    Parser.loop [] expHelp
        |> Parser.andThen
            (\list ->
                case list of
                    --"exp . ... . exp"
                    a1 :: tail ->
                        tail
                            |> List.foldl (\next f -> Apply f next)
                                (Apply e a1)
                            |> Parser.succeed

                    --else
                    [] ->
                        multiExp e
            )


parseExp : Parser Exp
parseExp =
    --"exp"
    singleExp
        |> Parser.andThen pipeExp


parseStatement : Parser Statement
parseStatement =
    Parser.oneOf
        [ Parser.succeed identity
            |. comment
            |= Parser.lazy (\_ -> parseStatement)
        , Parser.succeed Let
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
