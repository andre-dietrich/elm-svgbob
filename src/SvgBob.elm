module SvgBob exposing
    ( init, getSvg, default, getSvgWith
    , Model, Settings
    )

{-| Convert ASCII to SVG

It is a fork of Ivan Ceras example that is hosted at:

<https://github.com/ivanceras/elm-examples>

@docs init, getSvg, default, getSvgWith

@docs Model, Settings

-}

import Html exposing (Html)
import Svg exposing (Svg)
import SvgBob.Grid
import SvgBob.Model


{-| The model contains the whole ASCII String as well as some basic
configuration settings.
-}
type alias Model =
    SvgBob.Model.Model


{-| general settings ...

type alias Settings =
{ fontSize : Float
, lineWidth : Float
, textWidth : Float
, textHeight : Float
, arcRadius : Float
, color : String
, textColor : String
}

-}
type alias Settings =
    SvgBob.Model.Settings


{-| Default parameters to work with ...
-}
default : Settings
default =
    { fontSize = 14.0
    , lineWidth = 1.0
    , textWidth = 8.0
    , textHeight = 16.0
    , arcRadius = 4.0
    , strokeColor = "black"
    , textColor = "black"
    , backgroundColor = "white"
    }


{-| Pass a String and generate a Model with default settings
-}
init : Settings -> String -> Model
init =
    SvgBob.Model.init


{-| Get the resulting svg and pass it into a div or whatever
-}
getSvg : List (Svg.Attribute msg) -> Model -> Html msg
getSvg =
    SvgBob.Grid.getSvg Nothing


{-| Get the resulting svg and pass it into a div or parse it further or do whatever ...
-}
getSvgWith : (String -> Svg msg) -> List (Svg.Attribute msg) -> Model -> Html msg
getSvgWith verbatim =
    SvgBob.Grid.getSvg (Just verbatim)
