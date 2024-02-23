module Main exposing (..)

import Browser
import Example1 as Example
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (attribute, style, value)
import Html.Events exposing (onInput)
import SvgBob



{--code which detects lines and connections
also algorithm to connect these lines
--}


type alias Model =
    String


type Msg
    = Input String


view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ textarea [ onInput Input, value model, style "width" "50%" ] []
        , model
            |> SvgBob.getSvg SvgBob.default
                --With (\s -> Html.p [ Html.Attributes.style "height" "100%", Html.Attributes.style "width" "100%" ] [ Html.text s ])
                [ style "width" "50%"
                , attribute "vector-effect" "non-scaling-stroke"
                ]
        ]


update : Msg -> Model -> Model
update msg _ =
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
