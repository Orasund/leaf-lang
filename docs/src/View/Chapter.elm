module View.Chapter exposing (..)

import Data.Example exposing (Example)
import Data.Shared as Shared exposing (Model)
import Data.Test as Test
import Dict exposing (Dict)
import Element exposing (Element)
import ElmBook exposing (Msg)
import ElmBook.Chapter as Chapter
import ElmBook.ElmUI exposing (Chapter)
import View.Example as Example


view : String -> String -> Chapter Model
view title content =
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
    Chapter.chapter title
        |> Chapter.withStatefulComponentList compoentList
        |> Chapter.render content
