module Main exposing (..)

import Browser
import El.Core as Core
import El.Internal.Semantics as Semantics
import El.Internal.Syntax as Syntax
import El.Language exposing (Value(..))
import El.Util as Util
import Element
import Element.Input as Input
import Html exposing (Html)


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
      , text = "equal (equal 40 2) false"
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
                        |> Result.andThen (Semantics.eval Core.package)
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
