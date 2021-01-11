module SvgBob exposing
    ( getSvg, default, getSvgWith
    , Settings
    )

{-| Convert ASCII to SVG

It is a fork of Ivan Ceras example that is hosted at:

<https://github.com/ivanceras/elm-examples>

@docs getSvg, default, getSvgWith

@docs Settings

-}

import Html exposing (Html)
import Svg exposing (Svg)
import SvgBob.Grid
import SvgBob.Model
import SvgBob.Types exposing (Element(..))


{-| general settings ...

    type alias Settings =
        { fontSize : Float
        , lineWidth : Float
        , textWidth : Float
        , textHeight : Float
        , arcRadius : Float
        , strokeColor : String
        , textColor : String
        , backgroundColor : String
        , verbatim : Char
        , multilineVerbatim : Bool
        }

-}
type alias Settings =
    SvgBob.Model.Settings


{-| Default parameters to work with ...

    default =
        { fontSize = 14.0
        , lineWidth = 1.0
        , textWidth = 8.0
        , textHeight = 16.0
        , arcRadius = 4.0
        , strokeColor = "black"
        , textColor = "black"
        , backgroundColor = "white"
        , verbatim = '"'
        , multilineVerbatim = False
        }

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
    , verbatim = '"'
    , multilineVerbatim = False
    }


{-| Get the resulting svg and pass it into a div or whatever
-}
getSvg : Settings -> List (Svg.Attribute msg) -> String -> Html msg
getSvg settings attributes =
    SvgBob.Grid.getSvg settings attributes Nothing


{-| Get the resulting svg and pass it into a div or parse it further or do whatever ...
-}
getSvgWith : Settings -> List (Svg.Attribute msg) -> (String -> Svg msg) -> String -> Html msg
getSvgWith settings attributes verbatim =
    SvgBob.Grid.getSvg settings attributes (Just verbatim)
