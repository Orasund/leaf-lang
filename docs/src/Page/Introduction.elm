module Page.Introduction exposing (chapter)

import Data.Shared as Shared exposing (Model)
import Data.Test.Introduction as Test
import Dict
import ElmBook exposing (Msg)
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html exposing (Html)
import Leaf
import View.Example as Example


chapter : Chapter Model
chapter =
    let
        compoentList : List ( String, Model -> Html (Msg Model) )
        compoentList =
            Test.tests
                |> Dict.toList
                |> List.map
                    (\( name, example ) ->
                        ( name
                        , \model ->
                            if name == model.label then
                                Example.view
                                    { label = name
                                    , code = model.code
                                    , result = model.result
                                    , onChange = Shared.OnChange
                                    , onRun = Shared.OnRun
                                    }

                            else
                                Example.view
                                    { label = name
                                    , code = example.code
                                    , result = Nothing
                                    , onChange = Shared.OnChange
                                    , onRun = Shared.OnRun
                                    }
                        )
                    )
    in
    Chapter.chapter "Introduction"
        |> Chapter.withStatefulComponentList compoentList
        |> Chapter.render content


content : String
content =
    """

Leaf is a multi paradigm scripting language for Elm. It is designed to be extendable to fit your needs.

Leaf is
* dynamically Typed
* extendable (by normal Elm functions)
* based on Lua with a few features taken from Rust
* context sensitive (similar to Ports in Elm)
* small (50KB of pure Elm code)

## Hello World

A Hello World script in Leaf is as simple as

<component with-label="HelloWorld" />

In Elm you can evaluate it like this:

"""
        ++ Test.helloWorldString
        ++ """
"""
