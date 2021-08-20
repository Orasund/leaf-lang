module Main exposing (main)

import ElmBook exposing (Book)
import ElmBook.ThemeOptions as ThemeOptions
import Html
import Page.Introduction
import View.Logo as Logo


main : Book ()
main =
    ElmBook.book "Leaf"
        |> ElmBook.withThemeOptions
            [ ThemeOptions.subtitle "Scripting Language"
            , ThemeOptions.logo Logo.view
            , ThemeOptions.background "#73E2A7"
            , ThemeOptions.accent "black"
            , ThemeOptions.navBackground "#535657"
            , ThemeOptions.navAccent "#535657"
            , ThemeOptions.navAccentHighlight "white"
            , ThemeOptions.useHashBasedNavigation
            ]
        |> ElmBook.withChapters
            [ Page.Introduction.chapter
            ]
