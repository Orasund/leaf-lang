module View.Example exposing (view)

import Data.Shared as Shared
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ElmBook exposing (Msg)
import ElmBook.Actions as Actions
import Html
import Html.Attributes as Attributes
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
                    , Attributes.style "border-color" "white" |> Element.htmlAttribute
                    , Element.centerY
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
                        , Element.paddingXY 12 6
                        , Element.height <| Element.px 58
                        ]
                    |> Element.inFront
                , Attributes.class "elm-book-md__code elm-book-monospace elm-book-shadows-light"
                    |> Element.htmlAttribute
                , Border.rounded 6
                , Background.color <| Element.rgb255 42 53 77
                , Font.size 18
                , Border.width 0
                , Element.paddingXY 24 20
                , Element.width Element.fill
                , Attributes.style "color" "white" |> Element.htmlAttribute
                , Font.family
                    [ Font.typeface "Fira Code", Font.monospace ]
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
                            |> Element.el [ Element.width Element.fill ]
                    )
                |> Result.withDefault
                    (Html.pre [ Attributes.class "elm-book-md__code-default elm-book-monospace elm-book-shadows-light" ]
                        [ Html.text s ]
                        |> Element.html
                        |> Element.el [ Element.width Element.fill ]
                    )
    in
    Element.column
        [ Attributes.class "elm-book-md" |> Element.htmlAttribute
        , Element.width Element.fill
        , Element.centerX
        , Element.spacing 12
        , Element.paddingEach
            { top = 0
            , right = 0
            , bottom = 36
            , left = 0
            }
        ]
    <|
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
