module SvgBob exposing
    ( init, getSvg
    , Model
    )

{-| Convert ASCII to SVG

It is a fork of Ivan Ceras example that is hosted at:

<https://github.com/ivanceras/elm-examples>

@docs init, getSvg

@docs Model

-}

import Html exposing (Html)
import Svg
import SvgBob.Grid
import SvgBob.Model


{-| The model contains the whole ASCII String as well as some basic
configuration settings.
-}
type alias Model =
    SvgBob.Model.Model


{-| Pass a String and generate a Model with default settings
-}
init : String -> Model
init =
    SvgBob.Model.init


{-| Get the resulting svg and pass it into a div or whatever
-}
getSvg : List (Svg.Attribute msg) -> Model -> Html msg
getSvg =
    SvgBob.Grid.getSvg
