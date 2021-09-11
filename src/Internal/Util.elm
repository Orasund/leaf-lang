module Internal.Util exposing (valueToString)

import Dict
import Internal.Language exposing (Value(..))


valueToString : Value -> String
valueToString value =
    case value of
        NullVal ->
            "null"

        StringVal string ->
            "\"" ++ string ++ "\""

        BoolVal bool ->
            if bool then
                "true"

            else
                "false"

        IntVal int ->
            String.fromInt int

        FloatVal float ->
            String.fromFloat float

        ListVal list ->
            "["
                ++ (list |> List.map valueToString |> String.join ", ")
                ++ "]"

        ObjectVal dict ->
            "{"
                ++ (dict
                        |> Dict.toList
                        |> List.sortBy Tuple.first
                        |> List.map (\( k, v ) -> k ++ ": " ++ valueToString v)
                        |> String.join ", "
                   )
                ++ "}"

        FunctionVal _ _ ->
            "function"

        ExtensionVal _ ->
            "extension"



{--see https://github.com/elm/parser/pull/38
--}
