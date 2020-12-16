module SvgBob.Model exposing (Model, Settings, dim, init)

import Svg exposing (Svg)


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


type alias Model =
    { rows : Int
    , columns : Int
    , lines : List String
    , settings : Settings
    }


dim : List String -> ( Int, Int )
dim lines =
    ( List.length lines
    , lines
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0
    )


init : Settings -> String -> Model
init settings str =
    let
        lines =
            String.lines str

        ( rows, columns ) =
            dim lines
    in
    { rows = rows
    , columns = columns
    , lines = lines
    , settings = settings
    }
