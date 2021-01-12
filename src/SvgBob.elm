module SvgBob exposing
    ( getSvg, default, getSvgWith, getElements, drawElements
    , Settings, Configuration
    )

{-| Convert ASCII to SVG

It is a fork of Ivan Ceras example that is hosted at:

<https://github.com/ivanceras/elm-examples>

@docs getSvg, default, getSvgWith, getElements, drawElements

@docs Settings, Configuration

-}

import Html exposing (Html)
import Svg exposing (Svg)
import SvgBob.Grid
import SvgBob.Model
import SvgBob.Types exposing (Element(..), Point)


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
        , heightVerbatim : Maybe String
        , widthVerbatim : Maybe String
        }

The additional `heightVerbatim` and `widthVerbatim` can be used to overwrite the
calculated dimensions for that specific element. Otherwise the dimensions for
verbatim elements are calculated on the basis of the position and dimensions
of the strings within the ASCII-Art image.

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
        , heightVerbatim = Nothing
        , widthVerbatim = Nothing
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
    , heightVerbatim = Nothing
    , widthVerbatim = Nothing
    }


{-| This record is used to store all relevant data to draw an svg-image multiple
times, without reparsing it.
-}
type alias Configuration a =
    { svg : List ( Point, Element )
    , foreign : List ( a, ( Point, ( Int, Int ) ) )
    , settings : Settings
    , columns : Int
    , rows : Int
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


{-| Identified elements can be stored for later usage and verbatim code can be
exposed, so that it can be transformed into any other kind of representation.

The foreign part can be translated into anything, strings are not mandatory.
This way it can also be used within your model, if those foreign elements are
used to store more relevant information.

Use this function in conjunction with `drawElements`.

-}
getElements : Settings -> String -> Configuration String
getElements =
    SvgBob.Grid.getElements


{-| Use this to draw the result of the getElements function into an svg container.
The function that translates foreign objects into Svg elements is mandatory.
-}
drawElements : List (Svg.Attribute msg) -> (a -> Svg msg) -> Configuration a -> Html msg
drawElements =
    SvgBob.Grid.drawElements
