module Internal.Syntax exposing (parse)

import Dict exposing (Dict)
import Internal.Language exposing (Closure, Exp(..), Statement(..))
import Internal.Util as Util
import Parser exposing ((|.), (|=), Nestable(..), Parser, Step(..), Trailing(..))
import Set exposing (Set)


reserved : Set String
reserved =
    Set.fromList [ "let", "mut", "set", "fun", "null", "true", "false" ]


parseCapVar : Parser String
parseCapVar =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == ':'
        , reserved = reserved
        }


parseVariable : Parser String
parseVariable =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reserved
        }


{-| -}
stringHelper : Char -> Parser String
stringHelper quoteChar =
    let
        quoteString =
            String.fromChar quoteChar

        isNotEndOrEscape c =
            c /= quoteChar && c /= '\\'
    in
    Parser.succeed identity
        |. Parser.symbol quoteString
        |= Parser.loop []
            (\chunks ->
                Parser.oneOf
                    [ Parser.succeed (\chunk -> chunk :: chunks)
                        |= stringEscapeHelper [ quoteString ]
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse chunks |> String.join "")
                        |. Parser.token quoteString
                        |> Parser.map Parser.Done
                    , Parser.getChompedString (Parser.chompWhile isNotEndOrEscape)
                        |> Parser.andThen
                            (\s ->
                                if s == "" then
                                    Parser.problem ""

                                else
                                    Parser.succeed (s :: chunks)
                            )
                        |> Parser.map Parser.Loop
                    ]
            )


stringEscapeHelper : List String -> Parser String
stringEscapeHelper escapeChars =
    let
        mapString s =
            Parser.succeed s |. Parser.token s
    in
    Parser.succeed identity
        |. Parser.token "\\"
        |= Parser.oneOf
            (List.map mapString escapeChars
                ++ [ Parser.succeed "\n" |. Parser.token "n"
                   , Parser.succeed "\t" |. Parser.token "t"
                   , Parser.succeed "\u{000D}" |. Parser.token "r"
                   , Parser.succeed "\\" |. Parser.token "\\"
                   ]
            )


parseString : Parser String
parseString =
    Parser.oneOf
        [ stringHelper '"'
        , stringHelper '\''
        ]


comment : Parser ()
comment =
    Parser.spaces
        |. Parser.loop ()
            (\_ ->
                Parser.oneOf
                    [ Parser.lineComment "//"
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.multiComment "/*" "*/" Nestable
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map Parser.Done
                    ]
            )


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


functionExp : Parser Exp
functionExp =
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

        functionHelp : List String -> Parser (Step (List String) (List String))
        functionHelp revList =
            Parser.succeed identity
                |. internalOneOrMoreSpaces
                |= Parser.oneOf
                    [ Parser.succeed (\a -> Loop (a :: revList))
                        |= parseVariable
                    , Parser.succeed (Done (revList |> List.reverse))
                    ]
    in
    Parser.succeed identity
        |. Parser.keyword "fun"
        |. Parser.spaces
        |= parseVariable
        |> Parser.andThen
            (\a ->
                Parser.succeed (buildFunction a)
                    |= Parser.loop [ a ] functionHelp
                    |. Parser.spaces
                    |. Parser.keyword "->"
                    |. Parser.spaces
                    |= parseExp
            )


singleExp : Parser Exp
singleExp =
    Parser.succeed identity
        |. comment
        |= Parser.oneOf
            [ parseCapVar |> Parser.map Variable
            , Parser.lazy (\_ -> functionExp)
            , Parser.keyword "null" |> Parser.map (always NullExp)
            , parseString |> Parser.map StringExp
            , parseBool |> Parser.map BoolExp
            , parseNumber
            , Parser.lazy (\_ -> parseList) |> Parser.map ListExp
            , Parser.backtrackable <|
                Parser.succeed ClosureExp
                    |. Parser.symbol "("
                    |. Parser.spaces
                    |= Parser.lazy (\_ -> parseClosure)
                    |. Parser.spaces
                    |. Parser.symbol ")"
            , Parser.lazy (\_ -> parseObject) |> Parser.map ObjectExp
            ]


multiExp : Exp -> Parser Exp
multiExp e =
    let
        expHelp : List Exp -> Parser (Step (List Exp) (List Exp))
        expHelp revList =
            Parser.oneOf
                [ Parser.backtrackable <|
                    Parser.succeed (\a -> Loop (a :: revList))
                        |. internalOneOrMoreSpaces
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
                [ Parser.backtrackable <|
                    Parser.succeed (\a -> Loop (a :: revList))
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
        |> Parser.andThen multiExp
        |> Parser.andThen pipeExp


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
            |. Parser.keyword "set"
            |. Parser.spaces
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
            Parser.succeed identity
                |. comment
                |= Parser.oneOf
                    [ Parser.succeed (\stmt -> Loop (stmt :: revStmts))
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



--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------


internalOneOrMoreSpaces : Parser ()
internalOneOrMoreSpaces =
    Parser.oneOf
        [ Parser.symbol " "
        , Parser.symbol "\n"
        , Parser.symbol "\u{000D}"
        ]
