module Main exposing (main)

import Data.Shared as Shared exposing (Model)
import Element exposing (Element)
import ElmBook
import ElmBook.ComponentOptions as ComponentOptions
import ElmBook.Custom
import ElmBook.ElmUI exposing (Book)
import ElmBook.StatefulOptions as StatefulOptions
import ElmBook.ThemeOptions as ThemeOptions
import Html
import Html.Attributes as Attributes
import Page.Introduction
import Page.Statements
import Page.Types
import View.Logo as Logo
import View.Palette as Palette


header : Element msg
header =
    Html.span
        [ Attributes.class "elm-book-sans elm-book-header-default" ]
        [ Logo.view
        , Html.span [ Attributes.class "elm-book-header-default--wrapper" ]
            [ Html.span
                [ Attributes.class "elm-book-header-default--title" ]
                [ Html.text "Leaf" ]
            , Html.span
                [ Attributes.class "elm-book-header-default--subtitle" ]
                [ Html.text "Scripting Language" ]
            ]
        ]
        |> Element.html


main : Book Model
main =
    ElmBook.Custom.customBook (Element.layout []) "Leaf"
        |> ElmBook.withStatefulOptions
            [ StatefulOptions.initialState Shared.init
            ]
        |> ElmBook.withThemeOptions
            [ ThemeOptions.subtitle "Scripting Language"

            --, ThemeOptions.logo Logo.view
            , ThemeOptions.background Palette.green
            , ThemeOptions.header header
            , ThemeOptions.accent "black"
            , ThemeOptions.navBackground Palette.gray
            , ThemeOptions.navAccent Palette.gray
            , ThemeOptions.navAccentHighlight "white"
            , ThemeOptions.useHashBasedNavigation
            ]
        |> ElmBook.withComponentOptions
            [ ComponentOptions.displayInline ]
        |> ElmBook.withChapterGroups
            [ Page.Introduction.chapter
            , Page.Statements.chapter
            , Page.Types.chapter
            ]
