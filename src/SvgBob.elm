module SvgBob exposing (Model, getSvg, init)

import Html exposing (Html)
import SvgBob.Grid
import SvgBob.Model


type alias Model =
    SvgBob.Model.Model


init : String -> Model
init =
    SvgBob.Model.init


getSvg : Model -> Html msg
getSvg =
    SvgBob.Grid.getSvg
