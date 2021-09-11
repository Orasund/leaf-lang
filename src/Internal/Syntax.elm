module Internal.Syntax exposing (parse)

import Dict exposing (Dict)
import Internal.Language exposing (Closure, Exp(..), Statement(..))
import Internal.Util as Util
import Parser exposing (Parser)
import Parser.Char as Char
import Parser.Check as Check
import Parser.Common as Common
import Parser.Error as Error
import Parser.Sequence as Sequence
import Set exposing (Set)


reserved : Set String
reserved =
    Set.fromList [ "let", "mut", "set", "fun", "null", "true", "false" ]


parseCapVar : Parser String
parseCapVar =
    internalVariable
        { start = Char.letter
        , inner = [ Char.alphaNum, Char.char '_', Char.char ':' ]
        , reserved = reserved
        }


parseVariable : Parser String
parseVariable =
    internalVariable
        { start = Char.lowercase
        , inner = [ Char.alphaNum, Char.char '_' ]
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
        |> Parser.drop (Common.text quoteString)
        |> Parser.take
            (Parser.loop []
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
            )


stringEscapeHelper : List String -> Parser String
stringEscapeHelper escapeChars =
    let
        mapString s =
            Parser.succeed s
                |> Parser.drop (Common.text s)
    in
    Parser.succeed identity
        |> Parser.drop (Common.text "\\")
        |> Parser.take
            (Parser.oneOf
                (List.map mapString escapeChars
                    ++ [ Parser.succeed "\n" |> Parser.drop (Common.text "n")
                       , Parser.succeed "\t" |> Parser.drop (Common.text "t")
                       , Parser.succeed "\u{000D}" |> Parser.drop (Common.text "r")
                       , Parser.succeed "\\" |> Parser.drop (Common.text "\\")
                       , Parser.succeed "\"" |> Parser.drop (Common.text "\"")
                       ]
                )
            )


parseString : Parser String
parseString =
    stringHelper '"'
        |> Parser.into "String"


multiLineCommentHelper : Parser ()
multiLineCommentHelper =
    Parser.take
        (Sequence.until Parser.oneOf
            [ Common.text "*/"
            , Common.text "/*"
            ]
        )
        |> Parser.andThen
            (\_ string ->
                case string of
                    "/*" ->
                        Parser.lazy (\_ -> multiLineCommentHelper)

                    _ ->
                        Parser.succeed ()
            )


multiLineComment : Parser ()
multiLineComment =
    Parser.succeed
        |> Parser.drop (Common.text "/*")
        |> Parser.take multiLineCommentHelper
        |> Parser.take Common.spaces


comment : Parser ()
comment =
    Common.spaces
        |> Parser.take
            (Sequence.zeroOrMore
                (Parser.oneOf
                    [ Parser.succeed identity
                        |> Parser.drop (Common.text "//")
                        |> Parser.take Common.line
                        |> Parser.drop Common.spaces
                    , multiLineComment
                    ]
                )
            )
        |> Parser.into "Comment"


parseBool : Parser Bool
parseBool =
    Parser.oneOf
        [ Parser.succeed (always True)
            |> Parser.take Common.text "true"
        , Parser.succeed (always False)
            |> Parser.take Common.text "false"
        ]
        |> Parser.into "Boolean"


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
        |> Parser.into "Number"


parseList : Parser (List Exp)
parseList =
    internalSequence
        { start = '['
        , separator = ','
        , end = ']'
        , item = parseExp
        }
        |> Parser.into "List"


parseObjectField : Parser ( String, Exp )
parseObjectField =
    Parser.succeed Tuple.pair
        |> Parser.take parseVariable
        |> Parser.drop Common.token ':'
        |> Parser.take parseExp


parseObject : Parser (Dict String Exp)
parseObject =
    Parser.succeed Dict.fromList
        |> Parser.take
            (internalSequence
                { start = '{'
                , separator = ','
                , end = "}"
                , item = parseObjectField
                }
            )
        |> Parser.into "Object"


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
    in
    Parser.succeed identity
        |> Parser.drop (Common.token (Common.text "fun"))
        |> Parser.take parseVariable
        |> Parser.andThen
            (\a ->
                Parser.succeed (buildFunction a)
                    |> Parser.take
                        (Sequence.zeroOrMore
                            (Parser.succeed identity
                                |> Parser.drop (Sequence.oneOrMore Char.space)
                                |> Parser.take parseVariable
                            )
                        )
                    |> Parser.drop (Common.token (Common.text "->"))
                    |> Parser.take parseExp
            )
        |> Parser.into "Function"


singleExp : Parser Exp
singleExp =
    Parser.succeed identity
        |> Parser.drop comment
        |> Parser.take
            (Parser.oneOf
                [ parseCapVar |> Parser.map Variable
                , Parser.lazy (\_ -> functionExp)
                , Common.text "null" |> Parser.map (always NullExp)
                , parseString |> Parser.map StringExp
                , parseBool |> Parser.map BoolExp
                , parseNumber
                , Parser.lazy (\_ -> parseList) |> Parser.map ListExp
                , Parser.succeed ClosureExp
                    |> Parser.drop (Char.char '(')
                    |> Parser.drop Common.spaces
                    |> Parser.take (Parser.lazy (\_ -> parseBlock))
                    |> Parser.drop Common.spaces
                    |> Parser.drop (Char.char ')')
                , Parser.lazy (\_ -> parseObject) |> Parser.map ObjectExp
                ]
            )
        |> Parser.into "Expression"


multiExp : Exp -> Parser Exp
multiExp e =
    Sequence.zeroOrMore
        (Parser.succeed identity
            |> Parser.drop (Sequence.oneOrMore Char.space)
            |> Parser.take singleExp
        )
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
        |> Parser.into "Function Application"


pipeExp : Exp -> Parser Exp
pipeExp e =
    Sequence.zeroOrMore
        (Parser.succeed identity
            |> Parser.drop (Sequence.oneOrMore Char.space)
            |> Parser.drop Char.char '.'
            |> Common.spaces
            |> (singleExp |> Parser.andThen multiExp)
        )
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
        |> Parser.into "Expression Pipe"


parseExp : Parser Exp
parseExp =
    --"exp"
    singleExp
        |> Parser.andThen multiExp
        |> Parser.andThen pipeExp


parseStatement : Parser Statement
parseStatement =
    Parser.succeed identity
        |> Parser.take
            (Parser.oneOf
                [ Parser.succeed Let
                    |> Parser.drop (Common.text "let")
                    |> Parser.drop (Sequence.oneOrMore Char.space)
                    |> Parser.take parseVariable
                    |> Parser.drop (Common.token (Char.char '='))
                    |> Parser.take parseExp
                , Parser.succeed Mut
                    |> Parser.drop (Common.text "mut")
                    |> Parser.drop (Sequence.oneOrMore Char.space)
                    |> Parser.take parseVariable
                    |> Parser.drop (Common.token (Char.char '='))
                    |> Parser.take parseExp
                , Parser.succeed Set
                    |> Parser.drop (Common.text "set")
                    |> Parser.drop (Sequence.oneOrMore Char.space)
                    |> Parser.take parseVariable
                    |> Parser.drop (Common.token (Char.char '='))
                    |> Parser.take parseExp
                ]
            )
        |> Parser.drop Common.token (Char.char ';')
        |> Parser.into "Statement"


parseBlock : Parser Closure
parseBlock =
    Parser.succeed
        (\statements return ->
            { statements = statements
            , return = return
            }
        )
        |> Parser.take Sequence.zeroOrMore parseStatement
        |> Parser.take parseExp
        |> Parser.into "Block"


parse : String -> Result String Closure
parse =
    Parser.parse
        (Parser.succeed identity
            |> Parser.take parseBlock
            |> Parser.drop Common.spaces
            |> Parser.drop Check.end
        )
        >> Result.mapError Error.dumpCodeSnippet



--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------


internalSequence :
    { start : Char
    , separator : Char
    , end : Char
    , item : Parser a
    }
    -> List (Parser a)
internalSequence args =
    Parser.succeed (Maybe.withDefault [])
        |> Parser.drop (Common.token (Char.char args.start))
        |> Sequence.maybe
            (Parser.succeed (\a list -> a :: list)
                |> args.item
                |> Sequence.zeroOrMore
                    (Parser.succeed identity
                        |> Common.token (Char.char args.separator)
                        |> args.item
                    )
            )
        |> Parser.drop (Common.token (Char.char args.end))


internalVariable :
    { start : Parser Char
    , inner : List (Parser Char)
    , reserved : Set String
    }
    -> Parser String
internalVariable args =
    Parser.succeed (\a b -> String.cons a b)
        |> Parser.take args.start
        |> Parser.take (Parser.textOf (Sequence.oneOrMore args.inner))
