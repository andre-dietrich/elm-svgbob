module SvgBob exposing
    ( getSvg, defaultOptions, getSvgWith, getElements, drawElements
    , Settings, Configuration
    , setColors, setColorsIn
    )

{-| Convert ASCII to SVG

It is a fork of Ivan Ceras example that is hosted at:

<https://github.com/ivanceras/elm-examples>

@docs getSvg, defaultOptions, getSvgWith, getElements, drawElements

@docs Settings, Configuration

@docs setColors, setColorsIn

-}

import Html exposing (Html)
import Svg exposing (Svg)
import SvgBob.Grid
import SvgBob.Model exposing (Colors)
import SvgBob.Types exposing (Element(..), Point)


{-| general settings ...

    type alias Settings =
        { fontSize : Float
        , lineWidth : Float
        , textWidth : Float
        , textHeight : Float
        , arcRadius : Float
        , color :
            { stroke : String
            , text : String
            , background : String
            }
        , verbatim :
            { string : String
            , multiline : Bool
            , height : Maybe String
            , width : Maybe String
            }
        }

The additional `verbatim.height` and `verbatim.width` can be used to overwrite the
calculated dimensions for that specific element. Otherwise the dimensions for
verbatim elements are calculated on the basis of the position and dimensions
of the strings within the ASCII-Art image.

-}
type alias Settings =
    SvgBob.Model.Settings


{-| Default parameters to work with ...

    defaultOptions =
        { fontSize = 14.0
        , lineWidth = 1.0
        , textWidth = 8.0
        , textHeight = 16.0
        , arcRadius = 4.0
        , color =
            { stroke = "#222"
            , text = "black"
            , background = "white"
            }
        , verbatim =
            { string = "\""
            , multiline = False
            , height = Nothing
            , width = Nothing
            }
        }

-}
defaultOptions : Settings
defaultOptions =
    { fontSize = 14.0
    , lineWidth = 1.0
    , textWidth = 8.0
    , textHeight = 16.0
    , arcRadius = 4.0
    , color =
        { stroke = "#222"
        , text = "black"
        , background = "white"
        }
    , verbatim =
        { string = "\""
        , multiline = False
        , height = Nothing
        , width = Nothing
        }
    }


{-| This record is used to store all relevant data to draw an svg-image multiple
times, without re-parsing it.
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
getSvg : List (Svg.Attribute msg) -> String -> Settings -> Html msg
getSvg =
    SvgBob.Grid.getSvg Nothing


{-| Get the resulting svg and pass it into a div or parse it further or do whatever ...
-}
getSvgWith : (String -> Svg msg) -> List (Svg.Attribute msg) -> String -> Settings -> Html msg
getSvgWith =
    Just >> SvgBob.Grid.getSvg


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


{-| Helper for changing the color settings.

    settings
    |> setColors { stroke = "red", text = "green", background = "blue" }
    |> getSvg [] ...

-}
setColors : Colors -> Settings -> Settings
setColors color settings =
    { settings | color = color }


{-| Helper for changing the color configuration.

    config
    |> setColorsIn { stroke = "red", text = "green", background = "blue" }
    |> drawElements [] ...

-}
setColorsIn : Colors -> Configuration a -> Configuration a
setColorsIn color config =
    { config | settings = setColors color config.settings }
