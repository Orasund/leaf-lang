module El.Internal.Semantics exposing (eval)

import Dict exposing (Dict)
import El.Language exposing (Closure, Exp(..), Number(..), Statement(..), Value(..))
import El.Util


type Access
    = Read
    | ReadWrite


type alias Field =
    { value : Value
    , access : Access
    }



{--evalBuildin : BuildIn -> Dict String Field -> Result String Value
evalBuildin buildin context =
    case buildin of
        ------------------------------------------------------------------------
        --Null Type
        ------------------------------------------------------------------------
        IsNull exp ->
            context
                |> evalExp exp
                |> Result.map ((==) NullVal >> BoolVal)

        ------------------------------------------------------------------------
        --type BoolExp =
        ------------------------------------------------------------------------
        If exp1 exp2 exp3 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v ->
                        if v == BoolVal False then
                            context |> evalExp exp2

                        else
                            context |> evalExp exp3
                    )

        Eq exp1 exp2 ->
            BoolVal (exp1 == exp2) |> Ok

        IsBool exp ->
            context
                |> evalExp exp
                |> Result.map
                    (\v ->
                        case v of
                            BoolVal _ ->
                                BoolVal True

                            _ ->
                                BoolVal False
                    )

        ------------------------------------------------------------------------
        --type NumberExp =
        ------------------------------------------------------------------------
        Plus exp1 exp2 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v1 ->
                        context
                            |> evalExp exp2
                            |> Result.andThen
                                (\v2 ->
                                    case ( v1, v2 ) of
                                        ( NumberVal (IntNum n1), NumberVal (IntNum n2) ) ->
                                            NumberVal (IntNum (n1 + n2)) |> Ok

                                        ( NumberVal (FloatNum n1), NumberVal (FloatNum n2) ) ->
                                            NumberVal (FloatNum (n1 + n2)) |> Ok

                                        ( NumberVal n1, NumberVal n2 ) ->
                                            NumberVal (FloatNum (El.Util.numToFloat n1 + El.Util.numToFloat n2))
                                                |> Ok

                                        _ ->
                                            "Can't add "
                                                ++ El.Util.valueToString v1
                                                ++ " and "
                                                ++ El.Util.valueToString v2
                                                |> Err
                                )
                    )

        Mult exp1 exp2 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v1 ->
                        context
                            |> evalExp exp2
                            |> Result.andThen
                                (\v2 ->
                                    case ( v1, v2 ) of
                                        ( NumberVal (IntNum n1), NumberVal (IntNum n2) ) ->
                                            NumberVal (IntNum (n1 * n2)) |> Ok

                                        ( NumberVal n1, NumberVal n2 ) ->
                                            NumberVal (FloatNum (El.Util.numToFloat n1 * El.Util.numToFloat n2))
                                                |> Ok

                                        _ ->
                                            "Can't multiply "
                                                ++ El.Util.valueToString v1
                                                ++ " and "
                                                ++ El.Util.valueToString v2
                                                |> Err
                                )
                    )

        DivBy exp1 exp2 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v1 ->
                        context
                            |> evalExp exp2
                            |> Result.andThen
                                (\v2 ->
                                    case ( v1, v2 ) of
                                        ( NumberVal (IntNum n1), NumberVal (IntNum n2) ) ->
                                            NumberVal (IntNum (n2 // n1)) |> Ok

                                        ( NumberVal n1, NumberVal n2 ) ->
                                            El.Util.numToFloat n2
                                                / El.Util.numToFloat n1
                                                |> FloatNum
                                                |> NumberVal
                                                |> Ok

                                        _ ->
                                            "Can't divide "
                                                ++ El.Util.valueToString v2
                                                ++ " by "
                                                ++ El.Util.valueToString v1
                                                |> Err
                                )
                    )

        Floor exp ->
            context
                |> evalExp exp
                |> Result.andThen
                    (\v ->
                        case v of
                            NumberVal (IntNum _) ->
                                Ok v

                            NumberVal (FloatNum n) ->
                                NumberVal (IntNum (floor n)) |> Ok

                            _ ->
                                "Can't floor " ++ El.Util.valueToString v |> Err
                    )

        IsNumber exp ->
            context
                |> evalExp exp
                |> Result.map
                    (\v ->
                        case v of
                            NumberVal _ ->
                                BoolVal True

                            _ ->
                                BoolVal False
                    )

        IsInt exp ->
            context
                |> evalExp exp
                |> Result.map
                    (\v ->
                        case v of
                            NumberVal (IntNum _) ->
                                BoolVal True

                            _ ->
                                BoolVal False
                    )

        IsFloat exp ->
            context
                |> evalExp exp
                |> Result.map
                    (\v ->
                        case v of
                            NumberVal (FloatNum _) ->
                                BoolVal True

                            _ ->
                                BoolVal False
                    )

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

        IsList exp ->
            context
                |> evalExp exp
                |> Result.map
                    (\v ->
                        case v of
                            ListVal _ ->
                                BoolVal True

                            _ ->
                                BoolVal False
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

        IsObject exp ->
            context
                |> evalExp exp
                |> Result.map
                    (\v ->
                        case v of
                            ObjectVal _ ->
                                BoolVal True

                            _ ->
                                BoolVal False
                    )

        ------------------------------------------------------------------------
        -- FunctionExp
        ------------------------------------------------------------------------
        IsFunction exp ->
            context
                |> evalExp exp
                |> Result.map
                    (\v ->
                        case v of
                            FunctionVal _ _ ->
                                BoolVal True

                            _ ->
                                BoolVal False
                    )
--}


evalExp : Exp -> Dict String Field -> Result String Value
evalExp e context =
    case e of
        Variable string ->
            context
                |> Dict.get string
                |> Maybe.map (.value >> Ok)
                |> Maybe.withDefault (Err ("Can't find variable " ++ string))

        NullExp ->
            Ok NullVal

        StringExp string ->
            Ok <| StringVal string

        BoolExp bool ->
            Ok (BoolVal bool)

        NumberExp n ->
            Ok (NumberVal n)

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

        ClosureExp closure ->
            context
                |> Dict.map (\_ v -> { v | access = Read })
                |> evalClosure closure

        Apply exp1 exp2 ->
            context
                |> evalExp exp1
                |> Result.andThen
                    (\v1 ->
                        context
                            |> evalExp exp2
                            |> Result.andThen
                                (\v2 ->
                                    case v2 of
                                        FunctionVal string exp3 ->
                                            string
                                                |> Maybe.map
                                                    (\s ->
                                                        context
                                                            |> Dict.insert s { value = v1, access = Read }
                                                    )
                                                |> Maybe.withDefault context
                                                |> evalExp exp3

                                        _ ->
                                            "Can't apply a value to "
                                                ++ El.Util.valueToString v2
                                                |> Err
                                )
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


evalClosure : Closure -> Dict String Field -> Result String Value
evalClosure closure dict =
    closure.statements
        |> List.foldl (\statement -> Result.andThen (evalStatement statement))
            (Ok dict)
        |> Result.andThen (evalExp closure.return)


eval : Closure -> Result String Value
eval closure =
    evalClosure closure Dict.empty
