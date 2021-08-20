module Page.Introduction exposing (chapter)

import Data.Test.Introduction as Test
import Dict
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html exposing (Html)
import View.Example as Example


chapter : Chapter x
chapter =
    Chapter.chapter "Introduction"
        |> Chapter.withComponent component
        |> Chapter.render content


component : Html msg
component =
    Html.button [] [ Html.text "Click me!" ]


content : String
content =
    """

Leaf is a multi paradigm scripting language for Elm. 
It is designed to be extendable to fit your needs and it's multi paradigm nature allows anyone to use it.

---

## Hello World

A Hello World script in Leaf is as simple as

"""
        ++ Example.toString "HelloWorld" Test.tests
        ++ """
In Elm you can evaluate it like this:
"""
        ++ Test.helloWorldString
        ++ """
"""
