module Main exposing (..)

import Browser
import Element
import Element.Input as Input
import Html exposing (Html)
import Semantics
import Syntax
import Util exposing (Value(..))


type alias Model =
    { result : Result String Value
    , text : String
    }


type Msg
    = Updated String
    | Submit


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { result = Err ""
      , text = "(a) -> 42"
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Updated string ->
            ( { model | text = string }, Cmd.none )

        Submit ->
            ( { model
                | result =
                    model.text
                        |> Syntax.parse
                        |> Result.andThen Semantics.eval
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    [ Input.multiline [ Element.width <| Element.px 800, Element.height <| Element.px 400 ]
        { onChange = Updated
        , text = model.text
        , placeholder = Nothing
        , label = Input.labelHidden "text"
        , spellcheck = False
        }
    , Input.button []
        { onPress = Just Submit
        , label = Element.text "Interpret"
        }
    , (case model.result of
        Ok value ->
            Util.valueToString value

        Err string ->
            string
      )
        |> Element.text
    ]
        |> Element.column [ Element.centerX, Element.centerY, Element.spacing 8 ]
        |> Element.layout []
