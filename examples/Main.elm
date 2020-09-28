module Main exposing (..)

import Browser
import Example1
import Html exposing (Html, button, div, pre, text, textarea)
import Html.Attributes exposing (attribute, class, contenteditable, id, style, value)
import Html.Events exposing (onInput)
import Json.Decode exposing (string)
import SvgBob



{--code which detects lines and connections
also algorithmn to connect these lines
--}


type alias Model =
    String


type Msg
    = Input String


view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ Html.textarea [ onInput Input, value model, style "width" "50%" ] []
        , model
            |> SvgBob.init SvgBob.default
            |> SvgBob.getSvg
                [ style "width" "50%"
                , attribute "vector-effect" "non-scaling-stroke"
                ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input asciiText ->
            asciiText


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Example1.string
        , update = update
        , view = view
        }
