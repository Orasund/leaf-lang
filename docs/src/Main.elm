module Main exposing (main)

import Data.Shared as Shared exposing (Model, Msg)
import ElmBook
import ElmBook.ComponentOptions as ComponentOptions
import ElmBook.ElmUI as ElmBookUI exposing (Book)
import ElmBook.StatefulOptions as StatefulOptions
import ElmBook.ThemeOptions as ThemeOptions
import Page.Introduction
import View.Logo as Logo
import View.Palette as Palette


main : Book Model
main =
    ElmBookUI.book "Leaf"
        |> ElmBook.withStatefulOptions
            [ StatefulOptions.initialState Shared.init
            ]
        |> ElmBook.withThemeOptions
            [ ThemeOptions.subtitle "Scripting Language"
            , ThemeOptions.logo Logo.view
            , ThemeOptions.background Palette.green
            , ThemeOptions.accent "black"
            , ThemeOptions.navBackground Palette.gray
            , ThemeOptions.navAccent Palette.gray
            , ThemeOptions.navAccentHighlight "white"
            , ThemeOptions.useHashBasedNavigation
            ]
        |> ElmBook.withComponentOptions
            [ ComponentOptions.displayInline ]
        |> ElmBook.withChapters
            [ Page.Introduction.chapter
            ]
