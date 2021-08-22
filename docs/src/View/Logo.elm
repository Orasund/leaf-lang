module View.Logo exposing (view)

import Element exposing (Element)
import Svg
import Svg.Attributes as Attributes


view : Element msg
view =
    let
        size : Float
        size =
            6 * 6
    in
    Svg.svg
        [ Attributes.width <| String.fromFloat size
        , Attributes.height <| String.fromFloat size
        , Attributes.version "1.1"
        ]
        [ Svg.mask [ Attributes.id "myMask" ]
            [ Svg.path
                [ Attributes.d <|
                    "M 0 0 L 0 "
                        ++ String.fromFloat size
                        ++ " L "
                        ++ String.fromFloat size
                        ++ " "
                        ++ String.fromFloat size
                        ++ " L "
                        ++ String.fromFloat size
                        ++ " 0 Z"
                , Attributes.fill "white"
                ]
                []
            , Svg.path
                [ Attributes.d <|
                    "M "
                        ++ String.fromFloat (size / 6)
                        ++ ","
                        ++ String.fromFloat (size / 2)
                        ++ " A "
                        ++ String.fromFloat (size / 3)
                        ++ " "
                        ++ String.fromFloat (size / 3)
                        ++ ", 0, 1, 0, "
                        ++ String.fromFloat (size / 2)
                        ++ " "
                        ++ String.fromFloat (size / 6)
                        ++ " L "
                        ++ String.fromFloat (size / 6)
                        ++ " "
                        ++ String.fromFloat (size / 6)
                        ++ " Z"
                , Attributes.fill "black"
                ]
                []
            , Svg.path
                [ Attributes.d <|
                    "M "
                        ++ String.fromFloat (size / 2)
                        ++ " "
                        ++ String.fromFloat (size / 2)
                        ++ " L "
                        ++ String.fromFloat size
                        ++ " "
                        ++ String.fromFloat size
                        ++ " L 0 "
                        ++ String.fromFloat size
                        ++ " Z"
                , Attributes.fill "white"
                ]
                []
            ]
        , Svg.path
            [ Attributes.d <|
                "M 0,"
                    ++ String.fromFloat (size / 2)
                    ++ " A "
                    ++ String.fromFloat (size / 2)
                    ++ " "
                    ++ String.fromFloat (size / 2)
                    ++ ", 0, 1, 0, "
                    ++ String.fromFloat (size / 2)
                    ++ " 0 L 0 0 Z"
            , Attributes.fill "#000"
            , Attributes.mask "url(#myMask)"
            ]
            []
        ]
        |> Element.html
        |> Element.el
            [ Element.height <| Element.px <| round size
            , Element.width <| Element.px <| round size
            ]
