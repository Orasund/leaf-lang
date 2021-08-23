module Page.Introduction exposing (chapter)

import Data.Shared as Shared exposing (Model)
import Data.Test.Introduction as Test
import Dict
import Element exposing (Element)
import ElmBook exposing (Msg)
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter
import ElmBook.ElmUI as ElmBookUI exposing (Chapter)
import Html exposing (Html)
import Leaf
import View.Example as Example


chapter : Chapter Model
chapter =
    let
        compoentList : List ( String, Model -> Element (Msg Model) )
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

To run this script in Elm you need to call `Leaf.run`.

""" ++ Test.helloWorldString ++ """
The first argument of `Leaf.run` is the "context". 
You can pass values from Elm to Leaf through the context.
""" ++ Test.contextSensitiveString ++ """
Your Leaf script may now use the variable `name` and the extension function `append`.

<component with-label="Append" />

You can use the pipe operator `.` to pass a value from one function to the next.

<component with-label="PipeOp" />

Leaf provides you with a core library (Leaf.Core) containing the most essential functions (if, equal, isString and so on).
Any type specific functions, like the `append` function, must be implemented by yourself.
This has practical reasons: Maintaining a language and libraries for it is a lot of work. 
It's therefore better to split these more specific functions off into their own projects.
"""
