module SvgBob.Model exposing (Model, Settings, init)

import Array exposing (Array)
import Color


type alias Settings =
    { fontSize : Float
    , lineWidth : Float
    , textWidth : Float
    , textHeight : Float
    , arcRadius : Float
    , arcRadius : Float
    , color : Color.Color
    }


type alias Model =
    { rows : Int
    , columns : Int
    , lines : Array (Array Char)
    , settings : Settings
    }


init : String -> Model
init str =
    let
        lines =
            String.lines str

        max =
            List.map
                (\line ->
                    String.length line
                )
                lines
                |> List.maximum

        lineArr =
            Array.fromList lines

        lineChar =
            Array.map
                (\line ->
                    (String.toList <| String.trimRight line)
                        |> Array.fromList
                )
                lineArr
    in
    { rows = Array.length lineChar
    , columns = Maybe.withDefault 0 max
    , lines = lineChar
    , settings =
        { fontSize = 14.0
        , lineWidth = 1.0
        , textWidth = 8.0
        , textHeight = 16.0
        , arcRadius = 4.0
        , color = Color.rgb 0 0 0
        }
    }
