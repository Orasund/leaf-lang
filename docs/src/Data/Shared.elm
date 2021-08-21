module Data.Shared exposing (Model, Msg(..), init, update)

import Data.Example exposing (Example)
import Data.Test.Introduction as Introduction
import Dict exposing (Dict)
import ElmBook.Actions as Actions
import Leaf


tests : Dict String Example
tests =
    Introduction.tests


type alias Model =
    { label : String
    , code : String
    , result : Maybe String
    }


init : Model
init =
    { label = ""
    , code = ""
    , result = Nothing
    }


type Msg
    = OnChange String String
    | OnRun String


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnChange label code ->
            if label == model.label then
                { model | code = code, result = Nothing }

            else
                case tests |> Dict.get label of
                    Just example ->
                        { model | label = label, code = example.code, result = Nothing }

                    Nothing ->
                        model

        OnRun label ->
            if label == model.label then
                { model
                    | result =
                        Just <|
                            case model.code |> Leaf.run Dict.empty of
                                Ok ( value, _ ) ->
                                    Leaf.toString value

                                Err err ->
                                    err
                }

            else
                case tests |> Dict.get label of
                    Just example ->
                        { model
                            | label = label
                            , code = example.code
                            , result =
                                Just <|
                                    case example.code |> Leaf.run Dict.empty of
                                        Ok ( value, _ ) ->
                                            Leaf.toString value

                                        Err err ->
                                            err
                        }

                    Nothing ->
                        model
