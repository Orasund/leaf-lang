module View.Example exposing (view)

import Data.Shared as Shared
import ElmBook exposing (Msg)
import ElmBook.Actions as Actions
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Leaf exposing (Value)
import SyntaxHighlight
import View.Palette as Palette


view :
    { label : String
    , code : String
    , result : Maybe String
    , onChange : String -> String -> Shared.Msg
    , onRun : String -> Shared.Msg
    }
    -> Html (Msg Shared.Model)
view example =
    let
        input s =
            Html.div [ Attributes.style "position" "relative" ]
                [ Html.textarea
                    [ Attributes.class "elm-book-md__code elm-book-monospace elm-book-shadows-light"
                    , Attributes.style "width" "100%"
                    , Attributes.style "color" "white"
                    , Attributes.style "height" "auto"

                    --, Attributes.attribute "contenteditable" "True"
                    , Events.onInput
                        (example.onChange example.label
                            >> Actions.updateStateWith Shared.update
                        )
                    ]
                    [ Html.text s ]
                , Html.button
                    [ Attributes.style "position" "absolute"
                    , Attributes.style "right" "8px"
                    , Attributes.style "top" "8px"
                    , Attributes.style "background-color" Palette.green
                    , Attributes.style "padding" "10px"
                    , Attributes.style "font-size" "20px"
                    , Attributes.style "border-radius" "6px"
                    , Attributes.style "border-size" "4px"
                    , Attributes.style "border-color" "white"
                    , Events.onClick
                        (example.onRun example.label
                            |> Actions.updateStateWith Shared.update
                        )
                    ]
                    [ Html.text "run" ]
                ]

        code s =
            s
                |> SyntaxHighlight.noLang
                |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
                |> Result.map
                    (\content ->
                        Html.pre [ Attributes.class "elm-book-md__code elm-book-monospace elm-book-shadows-light" ]
                            [ content ]
                    )
                |> Result.withDefault
                    (Html.pre [ Attributes.class "elm-book-md__code-default elm-book-monospace elm-book-shadows-light" ]
                        [ Html.text s ]
                    )
    in
    Html.div [ Attributes.class "elm-book-md" ] <|
        input example.code
            :: (example.result
                    |> Maybe.map
                        (\c ->
                            [ Html.br [] []
                            , code c
                            ]
                        )
                    |> Maybe.withDefault []
               )



{--:: (example.result
                    |> Maybe.map
                        (\result ->
                            [ Html.br [] []
                            , result |> code
                            ]
                        )
                    |> Maybe.withDefault []
               )--}
