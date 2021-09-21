module Internal.Semantics exposing (Access(..), Field, eval)

import Dict exposing (Dict)
import Internal.Language as Language exposing (Block, Exp(..), Statement(..), Value(..))
import Internal.Util


type Access
    = Read
    | ReadWrite


type alias Field =
    { value : Language.Value
    , access : Access
    }



{--evalBuildin : BuildIn -> Dict String Field -> Result String Value
evalBuildin buildin context =
    case buildin of
        ------------------------------------------------------------------------
        --List
        ------------------------------------------------------------------------
        Head exp ->
            context
                |> evalExp exp
                |> Result.andThen
                    (\v ->
                        case v of
                            ListVal list ->
                                list
                                    |> List.head
                                    |> Maybe.withDefault NullVal
                                    |> Ok

                            _ ->
                                "Can't take the head of "
                                    ++ El.Util.valueToString v
                                    |> Err
                    )

        Tail exp ->
            context
                |> evalExp exp
                |> Result.andThen
                    (\v ->
                        case v of
                            ListVal list ->
                                list
                                    |> List.tail
                                    |> Maybe.map ListVal
                                    |> Maybe.withDefault NullVal
                                    |> Ok

                            _ ->
                                "Can't take the tail of "
                                    ++ El.Util.valueToString v
                                    |> Err
                    )

        Prepend exp1 exp2 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v1 ->
                        context
                            |> evalExp exp2
                            |> Result.andThen
                                (\v2 ->
                                    case v2 of
                                        ListVal list ->
                                            v1 :: list |> ListVal |> Ok

                                        _ ->
                                            "Can't prepend something to "
                                                ++ El.Util.valueToString v2
                                                |> Err
                                )
                    )

        Append exp1 exp2 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v1 ->
                        context
                            |> evalExp exp2
                            |> Result.andThen
                                (\v2 ->
                                    case ( v1, v2 ) of
                                        ( ListVal list1, ListVal list2 ) ->
                                            list2 ++ list1 |> ListVal |> Ok

                                        _ ->
                                            "Can't append "
                                                ++ El.Util.valueToString v1
                                                ++ " to "
                                                ++ El.Util.valueToString v2
                                                |> Err
                                )
                    )

        Length exp ->
            context
                |> evalExp exp
                |> Result.andThen
                    (\v ->
                        case v of
                            ListVal list ->
                                list
                                    |> List.length
                                    |> IntNum
                                    |> NumberVal
                                    |> Ok

                            _ ->
                                "Can't get the length of "
                                    ++ El.Util.valueToString v
                                    |> Err
                    )

        ------------------------------------------------------------------------
        --Object
        ------------------------------------------------------------------------
        Insert string exp1 exp2 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v1 ->
                        context
                            |> evalExp exp2
                            |> Result.andThen
                                (\v2 ->
                                    case v2 of
                                        ObjectVal dict ->
                                            dict
                                                |> Dict.insert string v1
                                                |> ObjectVal
                                                |> Ok

                                        _ ->
                                            "Can't insert a value into "
                                                ++ El.Util.valueToString v2
                                                |> Err
                                )
                    )

        Remove string exp ->
            context
                |> evalExp exp
                |> Result.andThen
                    (\v ->
                        case v of
                            ObjectVal dict ->
                                dict
                                    |> Dict.remove string
                                    |> ObjectVal
                                    |> Ok

                            _ ->
                                "Can't remove a value from "
                                    ++ El.Util.valueToString v
                                    |> Err
                    )

        Get string exp ->
            context
                |> evalExp exp
                |> Result.andThen
                    (\v ->
                        case v of
                            ObjectVal dict ->
                                dict
                                    |> Dict.get string
                                    |> Maybe.withDefault NullVal
                                    |> Ok

                            _ ->
                                "Can't remove a value from "
                                    ++ El.Util.valueToString v
                                    |> Err
                    )

        Size exp ->
            context
                |> evalExp exp
                |> Result.andThen
                    (\v ->
                        case v of
                            ObjectVal dict ->
                                dict
                                    |> Dict.size
                                    |> IntNum
                                    |> NumberVal
                                    |> Ok

                            _ ->
                                "Can't get the size of "
                                    ++ El.Util.valueToString v
                                    |> Err
                    )
--}


evalExp : Exp -> Dict String Field -> Result String Value
evalExp e context =
    Debug.log "result" <|
        case Debug.log "input" e of
            Variable string ->
                context
                    |> Dict.get string
                    |> Maybe.map (.value >> Ok)
                    |> Maybe.withDefault (Err ("Can't find variable " ++ string))

            NullExp ->
                Ok NullVal

            StringExp string ->
                Ok (StringVal string)

            BoolExp bool ->
                Ok (BoolVal bool)

            IntExp n ->
                Ok (IntVal n)

            FloatExp n ->
                Ok (FloatVal n)

            ListExp list ->
                list
                    |> List.foldl
                        (\a ->
                            Result.andThen
                                (\l ->
                                    context
                                        |> evalExp a
                                        |> Result.map (\b -> b :: l)
                                )
                        )
                        (Ok [])
                    |> Result.map (List.reverse >> ListVal)

            ObjectExp dict ->
                dict
                    |> Dict.foldl
                        (\k a ->
                            Result.andThen
                                (\l ->
                                    context
                                        |> evalExp a
                                        |> Result.map (\b -> l |> Dict.insert k b)
                                )
                        )
                        (Ok Dict.empty)
                    |> Result.map ObjectVal

            FunctionExp maybeString exp ->
                Ok (FunctionVal maybeString exp)

            BlockExp block ->
                context
                    |> Debug.log "context"
                    |> Dict.map (\_ v -> { v | access = Read })
                    |> evalBlock (Debug.log "block" block)
                    |> Result.map Tuple.first

            Apply exp1 exp2 ->
                context
                    |> evalExp exp2
                    |> Result.andThen
                        (\v2 ->
                            case v2 of
                                FunctionVal string exp3 ->
                                    context
                                        |> evalExp
                                            (string
                                                |> Maybe.map
                                                    (\s ->
                                                        exp3
                                                            |> substitute s exp1
                                                    )
                                                |> Maybe.withDefault exp3
                                            )

                                ExtensionVal fun ->
                                    context
                                        |> evalExp exp1
                                        |> Result.andThen
                                            (\v1 ->
                                                fun v1
                                            )

                                _ ->
                                    "Can't apply a value to "
                                        ++ Internal.Util.valueToString v2
                                        |> Err
                        )



{--BuildInFun buildin ->
            context
                |> evalBuildin buildin--}


evalStatement : Statement -> Dict String Field -> Result String (Dict String Field)
evalStatement statement context =
    case statement of
        Let string exp ->
            if context |> Dict.member string then
                Err <| "Variable " ++ string ++ " is already defined"

            else
                context
                    |> evalExp exp
                    |> Result.map
                        (\v ->
                            context
                                |> Dict.insert string { value = v, access = Read }
                        )

        Mut string exp ->
            if context |> Dict.member string then
                Err <| "Variable " ++ string ++ " is already defined"

            else
                context
                    |> evalExp exp
                    |> Result.map
                        (\v ->
                            context |> Dict.insert string { value = v, access = ReadWrite }
                        )

        Set string exp ->
            case context |> Dict.get string |> Maybe.map .access of
                Just ReadWrite ->
                    context
                        |> evalExp exp
                        |> Result.map
                            (\v ->
                                context |> Dict.insert string { value = v, access = ReadWrite }
                            )

                Just Read ->
                    Err <| "Variable " ++ string ++ " is not mutable"

                Nothing ->
                    Err <| "Variable " ++ string ++ " is not defined"


evalBlock : Block -> Dict String Field -> Result String ( Value, Dict String Field )
evalBlock block context =
    block.statements
        |> List.foldl (\statement -> Result.andThen (evalStatement statement))
            (Ok context)
        |> Result.andThen (\d -> d |> evalExp block.return |> Result.map (\v -> ( v, d )))


eval : Dict String Field -> Block -> Result String ( Value, Dict String Field )
eval dict closure =
    evalBlock closure dict


substitute : String -> Exp -> Exp -> Exp
substitute var with exp =
    case exp of
        Variable string ->
            if string == var then
                with

            else
                Variable string

        NullExp ->
            exp

        StringExp _ ->
            exp

        BoolExp _ ->
            exp

        IntExp _ ->
            exp

        FloatExp _ ->
            exp

        ListExp list ->
            list
                |> List.map (substitute var with)
                |> ListExp

        ObjectExp dict ->
            dict
                |> Dict.map (\_ -> substitute var with)
                |> ObjectExp

        FunctionExp maybe exp2 ->
            FunctionExp maybe (exp2 |> substitute var with)

        BlockExp { statements, return } ->
            BlockExp
                { statements = statements
                , return = return |> substitute var with
                }

        Apply exp2 exp3 ->
            Apply (exp2 |> substitute var with) (exp3 |> substitute var with)
