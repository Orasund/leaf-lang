module Internal.Util exposing (deadEndsToString, numToFloat, valueToString)

import Dict
import Internal.Language exposing (Number(..), Value(..))
import Parser exposing (DeadEnd, Problem(..))


numToFloat : Number -> Float
numToFloat v =
    case v of
        IntNum n ->
            toFloat n

        FloatNum n ->
            n


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

        NumberVal number ->
            case number of
                IntNum int ->
                    String.fromInt int

                FloatNum float ->
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


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    let
        deadEndToString : DeadEnd -> String
        deadEndToString deadEnd =
            let
                position : String
                position =
                    "row:" ++ String.fromInt deadEnd.row ++ " col:" ++ String.fromInt deadEnd.col ++ "\n"
            in
            "Parser error: "
                ++ (case deadEnd.problem of
                        Expecting str ->
                            "Expecting " ++ str ++ " at " ++ position

                        ExpectingInt ->
                            "Expecting Int at " ++ position

                        ExpectingHex ->
                            "Expecting Hex at " ++ position

                        ExpectingOctal ->
                            "Expecting Octal at " ++ position

                        ExpectingBinary ->
                            "Expecting Binary at " ++ position

                        ExpectingFloat ->
                            "Expecting Float at " ++ position

                        ExpectingNumber ->
                            "Expecting Number at " ++ position

                        ExpectingVariable ->
                            "Expecting Variable at " ++ position

                        ExpectingSymbol str ->
                            "Expecting Symbol " ++ str ++ " at " ++ position

                        ExpectingKeyword str ->
                            "Expecting Keyword " ++ str ++ " at " ++ position

                        ExpectingEnd ->
                            "Expecting End at " ++ position

                        UnexpectedChar ->
                            "Unexpected Char at " ++ position

                        Problem str ->
                            "Problem String " ++ str ++ " at " ++ position

                        BadRepeat ->
                            "Bad Repeat at " ++ position
                   )
    in
    List.foldl (++) "" (List.map deadEndToString deadEnds)
