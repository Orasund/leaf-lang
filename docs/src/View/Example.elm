module View.Example exposing (view)

import Data.Shared as Shared
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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
    -> Element (Msg Shared.Model)
view example =
    let
        input s =
            Input.multiline
                [ Input.button
                    [ Attributes.style "background-color" Palette.green |> Element.htmlAttribute
                    , Element.padding 10
                    , Font.size 20
                    , Border.rounded 6
                    , Border.width 4
                    , Attributes.style "border-color" "white" |> Element.htmlAttribute
                    ]
                    { onPress =
                        example.onRun example.label
                            |> Actions.updateStateWith Shared.update
                            |> Just
                    , label = Element.text "run"
                    }
                    |> Element.el
                        [ Element.alignRight
                        , Element.alignTop
                        , Element.padding 8
                        ]
                    |> Element.above
                , Attributes.class "elm-book-md__code elm-book-monospace elm-book-shadows-light"
                    |> Element.htmlAttribute
                , Element.width Element.fill
                , Attributes.style "color" "white" |> Element.htmlAttribute
                ]
                { onChange =
                    example.onChange example.label
                        >> Actions.updateStateWith Shared.update
                , text = s
                , placeholder = Nothing
                , label = Input.labelHidden "leaf script"
                , spellcheck = False
                }

        code s =
            s
                |> SyntaxHighlight.noLang
                |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
                |> Result.map
                    (\content ->
                        Html.pre [ Attributes.class "elm-book-md__code elm-book-monospace elm-book-shadows-light" ]
                            [ content ]
                            |> Element.html
                    )
                |> Result.withDefault
                    (Html.pre [ Attributes.class "elm-book-md__code-default elm-book-monospace elm-book-shadows-light" ]
                        [ Html.text s ]
                        |> Element.html
                    )
    in
    Element.column [ Attributes.class "elm-book-md" |> Element.htmlAttribute ] <|
        input example.code
            :: (example.result
                    |> Maybe.map
                        (\c ->
                            [ code c
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
