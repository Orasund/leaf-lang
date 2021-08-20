module View.Example exposing (toString)

import Data.Example exposing (Example)
import Dict exposing (Dict)
import Leaf


toString : String -> Dict String Example -> String
toString name dict =
    case dict |> Dict.get name of
        Just example ->
            "\n```\n "
                ++ example.code
                ++ "\n```\n\n```\n ==> "
                ++ Leaf.toString example.result
                ++ "\n```\n"

        Nothing ->
            "``` Error: Example with name \""
                ++ name
                ++ "\" was not found.\\"
                ++ "If you're seeing this message, please contact the maintainer of the package.```"
