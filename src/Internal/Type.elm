module Internal.Type exposing (Type(..), any, bool, dict, extension, float, function, int, is, list, null, nullable, number, record, string, stringLiteral, union)

import Dict exposing (Dict)
import Internal.Language exposing (Exp, Number(..), Value(..))
import Internal.Util as Util
import List
import Set exposing (Set)


type Type
    = AnyType
    | NullType
    | NullableType Type
    | StringType
    | StringLiteralType String
    | BoolType
    | IntType
    | FloatType
    | NumberType
    | ListType Type
    | DictType Type
    | RecordType (Dict String Type)
    | FunctionType Type Type
    | ExtensionType (Type -> Type)
    | UnionType (Set Type)


any : Value -> Result String Value
any v =
    Ok <| v


null : Value -> Result String ()
null v =
    case v of
        NullVal ->
            Ok <| ()

        _ ->
            Err <| "I expected null but got " ++ Util.valueToString v ++ " instead."


nullable : (Value -> Result String a) -> Value -> Result String (Maybe a)
nullable mapper v0 =
    case v0 of
        NullVal ->
            Ok Nothing

        _ ->
            mapper v0
                |> Result.map Just


string : Value -> Result String String
string v =
    case v of
        StringVal s ->
            Ok <| s

        _ ->
            Err <| "I expected a symbol but got " ++ Util.valueToString v ++ " instead."


stringLiteral : String -> Value -> Result String ()
stringLiteral s0 v =
    case v of
        StringVal s ->
            if s == s0 then
                Ok <| ()

            else
                Err <| "I expected the string " ++ Util.valueToString (StringVal s0) ++ " but got " ++ Util.valueToString v ++ " instead."

        _ ->
            Err <| "I expected a symbol but got " ++ Util.valueToString v ++ " instead."


bool : Value -> Result String Bool
bool v =
    case v of
        BoolVal a ->
            Ok <| a

        _ ->
            Err <| "I expected a boolean but got " ++ Util.valueToString v ++ " instead."


number : Value -> Result String Number
number v =
    case v of
        NumberVal a ->
            Ok <| a

        _ ->
            Err <| "I expected a number but got " ++ Util.valueToString v ++ " instead."


int : Value -> Result String Int
int v =
    case v of
        NumberVal (IntNum a) ->
            Ok <| a

        _ ->
            Err <| "I expected a integer but got " ++ Util.valueToString v ++ " instead."


float : Value -> Result String Float
float v =
    case v of
        NumberVal (FloatNum a) ->
            Ok <| a

        _ ->
            Err <| "I expected a float but got " ++ Util.valueToString v ++ " instead."


list : (Value -> Result String a) -> Value -> Result String (List a)
list mapper v0 =
    case v0 of
        ListVal l ->
            l
                |> List.foldl
                    (\v result ->
                        result
                            |> Result.andThen
                                (\l2 ->
                                    v
                                        |> mapper
                                        |> Result.map (\a -> a :: l2)
                                )
                    )
                    (Ok [])
                |> Result.map List.reverse

        _ ->
            Err <| "I expected a list but got " ++ Util.valueToString v0 ++ " instead."


dict : (Value -> Result String a) -> Value -> Result String (Dict String a)
dict mapper v0 =
    case v0 of
        ObjectVal d ->
            d
                |> Dict.foldl
                    (\k v result ->
                        result
                            |> Result.andThen
                                (\l2 ->
                                    v
                                        |> mapper
                                        |> Result.map (\a -> l2 |> Dict.insert k a)
                                )
                    )
                    (Ok Dict.empty)

        _ ->
            Err <| "I expected an object but got " ++ Util.valueToString v0 ++ " instead."


record : Dict String Type -> Value -> Result String (Dict String Value)
record d0 v0 =
    case v0 of
        ObjectVal d ->
            d0
                |> Dict.foldl
                    (\k t result ->
                        result
                            |> Result.andThen
                                (\l2 ->
                                    d
                                        |> Dict.get k
                                        |> Maybe.map
                                            (\v ->
                                                v
                                                    |> is t
                                                    |> Result.map (always (l2 |> Dict.insert k v))
                                            )
                                        |> Maybe.withDefault (Err <| "I could not find a field with key " ++ k ++ " in " ++ Util.valueToString v0 ++ ".")
                                )
                    )
                    (Ok Dict.empty)

        _ ->
            Err <| "I expected an object but got " ++ Util.valueToString v0 ++ " instead."


function : Value -> Result String ( Maybe String, Exp )
function v0 =
    case v0 of
        FunctionVal s e ->
            Ok ( s, e )

        _ ->
            Err <| "I expected a function but got " ++ Util.valueToString v0 ++ " instead."


extension : Value -> Result String (Value -> Result String Value)
extension v0 =
    case v0 of
        ExtensionVal e ->
            Ok e

        _ ->
            Err <| "I expected an extension function but got " ++ Util.valueToString v0 ++ " instead."


union : List Type -> Value -> Result String ()
union l v0 =
    let
        r =
            l
                |> List.map (\t -> is t v0)
    in
    if
        r
            |> List.any
                (\result ->
                    case result of
                        Ok _ ->
                            True

                        Err _ ->
                            False
                )
    then
        Ok ()

    else
        r
            |> List.filterMap
                (\result ->
                    case result of
                        Ok _ ->
                            Nothing

                        Err s ->
                            Just s
                )
            |> String.join " "
            |> Err


internalIsNullable : Type -> Value -> Result String ()
internalIsNullable t v =
    case t of
        AnyType ->
            Ok ()

        NullType ->
            nullable null v |> Result.map (always ())

        NullableType a ->
            nullable (internalIsNullable a) v |> Result.map (always ())

        StringType ->
            nullable string v |> Result.map (always ())

        StringLiteralType s ->
            nullable (stringLiteral s) v |> Result.map (always ())

        BoolType ->
            nullable bool v |> Result.map (always ())

        IntType ->
            nullable int v |> Result.map (always ())

        FloatType ->
            nullable float v |> Result.map (always ())

        NumberType ->
            nullable number v |> Result.map (always ())

        ListType t2 ->
            nullable (internalIsList t2) v |> Result.map (always ())

        DictType t2 ->
            nullable (internalIsDict t2) v |> Result.map (always ())

        RecordType d ->
            nullable (record d) v |> Result.map (always ())

        FunctionType _ _ ->
            nullable function v |> Result.map (always ())

        ExtensionType _ ->
            nullable extension v |> Result.map (always ())

        UnionType s ->
            nullable (union (s |> Set.toList)) v |> Result.map (always ())


internalIsList : Type -> Value -> Result String ()
internalIsList t v =
    case t of
        AnyType ->
            Ok ()

        NullType ->
            list null v |> Result.map (always ())

        NullableType a ->
            list (internalIsNullable a) v |> Result.map (always ())

        StringType ->
            list string v |> Result.map (always ())

        StringLiteralType s ->
            list (stringLiteral s) v |> Result.map (always ())

        BoolType ->
            list bool v |> Result.map (always ())

        IntType ->
            list int v |> Result.map (always ())

        FloatType ->
            list float v |> Result.map (always ())

        NumberType ->
            list number v |> Result.map (always ())

        ListType t2 ->
            list (internalIsList t2) v |> Result.map (always ())

        DictType t2 ->
            list (internalIsDict t2) v |> Result.map (always ())

        RecordType d ->
            list (record d) v |> Result.map (always ())

        FunctionType _ _ ->
            list function v |> Result.map (always ())

        ExtensionType _ ->
            list extension v |> Result.map (always ())

        UnionType s ->
            list (union (s |> Set.toList)) v |> Result.map (always ())


internalIsDict : Type -> Value -> Result String ()
internalIsDict t v =
    case t of
        AnyType ->
            Ok ()

        NullType ->
            dict null v |> Result.map (always ())

        NullableType a ->
            dict (internalIsNullable a) v |> Result.map (always ())

        StringType ->
            dict string v |> Result.map (always ())

        StringLiteralType s ->
            dict (stringLiteral s) v |> Result.map (always ())

        BoolType ->
            dict bool v |> Result.map (always ())

        IntType ->
            dict int v |> Result.map (always ())

        FloatType ->
            dict float v |> Result.map (always ())

        NumberType ->
            dict number v |> Result.map (always ())

        ListType t2 ->
            dict (internalIsList t2) v |> Result.map (always ())

        DictType t2 ->
            dict (internalIsDict t2) v |> Result.map (always ())

        RecordType d ->
            dict (record d) v |> Result.map (always ())

        FunctionType _ _ ->
            dict function v |> Result.map (always ())

        ExtensionType _ ->
            dict extension v |> Result.map (always ())

        UnionType s ->
            dict (union (s |> Set.toList)) v |> Result.map (always ())


is : Type -> Value -> Result String ()
is t v =
    case t of
        AnyType ->
            Ok ()

        NullType ->
            null v |> Result.map (always ())

        NullableType a ->
            internalIsNullable a v |> Result.map (always ())

        StringType ->
            string v |> Result.map (always ())

        StringLiteralType s ->
            stringLiteral s v |> Result.map (always ())

        BoolType ->
            bool v |> Result.map (always ())

        IntType ->
            int v |> Result.map (always ())

        FloatType ->
            float v |> Result.map (always ())

        NumberType ->
            number v |> Result.map (always ())

        ListType t2 ->
            internalIsList t2 v

        DictType t2 ->
            internalIsDict t2 v

        RecordType d ->
            record d v |> Result.map (always ())

        FunctionType _ _ ->
            function v |> Result.map (always ())

        ExtensionType _ ->
            extension v |> Result.map (always ())

        UnionType s ->
            union (s |> Set.toList) v |> Result.map (always ())
