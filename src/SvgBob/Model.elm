module SvgBob.Model exposing (Model, Settings, init)

import Array exposing (Array)
import Color


type alias Settings =
    { fontSize : Float
    , lineWidth : Float
    , textWidth : Float
    , textHeight : Float
    , arcRadius : Float
    , color : Color.Color
    }


type alias Model =
    { rows : Int
    , columns : Int
    , lines : List (List Char)
    , settings : Settings
    }


init : String -> Model
init str =
    let
        lines =
            String.lines str

        max =
            lines
                |> List.map String.length
                |> List.maximum

        lineChar =
            List.map (String.trimRight >> String.toList) lines
    in
    { rows = List.length lineChar
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
