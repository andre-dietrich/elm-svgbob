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
    , lines : List String
    , settings : Settings
    }


init : Settings -> String -> Model
init settings str =
    let
        lines =
            String.lines str

        max =
            lines
                |> List.map String.length
                |> List.maximum
    in
    { rows = List.length lines
    , columns = Maybe.withDefault 0 max
    , lines = lines
    , settings = settings
    }
