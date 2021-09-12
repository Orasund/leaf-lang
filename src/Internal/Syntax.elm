module Internal.Syntax exposing (parse)

import Dict exposing (Dict)
import Internal.Language exposing (Closure, Exp(..), Statement(..))
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
    Parser.oneOf
        [ Parser.succeed identity
            |> Parser.drop (Char.char '\\')
            |> Parser.take stringEscapeHelper
        , [ Char.char quoteChar
          , Char.char '\\'
          ]
            |> Parser.oneOf
            |> Char.except
            |> Sequence.zeroOrMore
            |> Parser.textOf
        ]


stringEscapeHelper : Parser String
stringEscapeHelper =
    Parser.succeed identity
        |> Parser.take
            (Parser.oneOf
                [ Parser.succeed "\\\n" |> Parser.drop (Char.char 'n')
                , Parser.succeed "\\\t" |> Parser.drop (Char.char 't')
                , Parser.succeed "\\\u{000D}" |> Parser.drop (Char.char 'r')
                , Parser.succeed "\\\\" |> Parser.drop (Char.char '\\')
                , Parser.succeed "\\\"" |> Parser.drop (Char.char '"')
                ]
            )


parseString : Parser String
parseString =
    Parser.succeed identity
        |> Parser.drop (Char.char '"')
        |> Parser.take (stringHelper '"')
        |> Parser.drop (Char.char '"')
        |> Parser.into "String"



{--multiLineCommentHelper : Parser ()
multiLineCommentHelper =
    Paser.succeed identity
        |> Parser.drop (Common.text "/*")
        |> Parser.drop
            (Sequence.until
                (Parser.oneOf
                    [ Common.text "*/" |> Parser.map (\_ -> False)
                    , Common.text "/*" |> Parser.map (\_ -> True)
                    ]
                )
                |> Parser.andThen
                    (\( _, isStart ) ->
                        if isStart then
                            Parser.lazy (\_ -> multiLineCommentHelper)

                        else
                            Parser.succeed ()
                    )
            )
        |> Parser.drop (Common.text "*/")


multiLineComment : Parser ()
multiLineComment =
    Parser.succeed
        |> Parser.take multiLineCommentHelper
        |> Parser.take Common.spaces--}


comment : Parser ()
comment =
    Parser.succeed ()
        |> Parser.drop
            (Sequence.zeroOrMore
                (Parser.oneOf
                    [ Parser.succeed identity
                        |> Parser.drop (Common.text "//")
                        |> Parser.take Common.line
                        |> Parser.drop Common.spaces

                    --, multiLineComment
                    ]
                )
            )
        |> Parser.into "Comment"


parseBool : Parser Bool
parseBool =
    Parser.oneOf
        [ Parser.succeed (always True)
            |> Parser.take (Common.text "true")
        , Parser.succeed (always False)
            |> Parser.take (Common.text "false")
        ]
        |> Parser.into "Boolean"


parseNumber : Parser Exp
parseNumber =
    Parser.oneOf
        [ Parser.succeed IntExp
            |> Parser.take Common.int
        , Parser.succeed FloatExp
            |> Parser.take Common.number
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
        |> Parser.drop (Common.token (Char.char ':'))
        |> Parser.take parseExp


parseObject : Parser (Dict String Exp)
parseObject =
    Parser.succeed Dict.fromList
        |> Parser.take
            (internalSequence
                { start = '{'
                , separator = ','
                , end = '}'
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
        |> Parser.drop (Common.text "fun")
        |> Parser.drop Common.spaces
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
        |> Parser.drop (Sequence.maybe comment)
        |> Parser.take
            (Parser.oneOf
                [ Parser.lazy (\_ -> functionExp)
                , Common.text "null" |> Parser.map (always NullExp)
                , parseString |> Parser.map StringExp
                , parseBool |> Parser.map BoolExp
                , parseCapVar |> Parser.map Variable
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
            |> Parser.drop (Char.char '.')
            |> Parser.drop Common.spaces
            |> Parser.take (singleExp |> Parser.andThen multiExp)
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
        |> Parser.drop (Sequence.maybe comment)
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
        |> Parser.drop Common.spaces
        |> Parser.drop (Char.char ';')
        |> Parser.drop Common.spaces
        |> Parser.into "Statement"


parseBlock : Parser Closure
parseBlock =
    Parser.succeed
        (\statements return ->
            { statements = statements
            , return = return
            }
        )
        |> Parser.take (Sequence.zeroOrMore parseStatement)
        |> Parser.take parseExp
        |> Parser.into "Block"


parse : String -> Result String Closure
parse string =
    Parser.parse string
        (Parser.succeed identity
            |> Parser.take parseBlock
            |> Parser.drop Common.spaces
            |> Parser.drop Check.end
        )
        |> Result.mapError (Error.dump "" >> String.join "\n")



--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------


internalSequence :
    { start : Char
    , separator : Char
    , end : Char
    , item : Parser a
    }
    -> Parser (List a)
internalSequence args =
    Parser.succeed (Maybe.withDefault [])
        |> Parser.drop (Char.char args.start)
        |> Parser.drop Common.spaces
        |> Parser.take
            (Sequence.maybe
                (Parser.succeed (\a list -> a :: list)
                    |> Parser.take args.item
                    |> Parser.take
                        (Sequence.zeroOrMore
                            (Parser.succeed identity
                                |> Parser.drop (Common.token (Char.char args.separator))
                                |> Parser.take args.item
                            )
                        )
                )
            )
        |> Parser.drop Common.spaces
        |> Parser.drop (Char.char args.end)


internalVariable :
    { start : Parser Char
    , inner : List (Parser Char)
    , reserved : Set String
    }
    -> Parser String
internalVariable args =
    Parser.oneOf
        [ args.reserved
            |> Set.toList
            |> List.map
                (\string ->
                    Parser.succeed identity
                        |> Parser.take (Common.text string)
                        |> Parser.drop Check.wordBoundary
                )
            |> Parser.oneOf
        , Parser.succeed (\a b -> String.cons a b)
            |> Parser.take args.start
            |> Parser.take
                (args.inner
                    |> Parser.oneOf
                    |> Sequence.oneOrMore
                    |> Parser.textOf
                )
        ]
