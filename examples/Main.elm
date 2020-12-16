module Main exposing (..)

import Browser
import Example0 as Example
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
            |> SvgBob.getSvgWith (\s -> Html.p [ Html.Attributes.style "height" "100%", Html.Attributes.style "width" "100%" ] [ Html.text s ])
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
        { init = Example.string
        , update = update
        , view = view
        }
