module SvgBob.Model exposing
    ( Colors
    , Model
    , Settings
    , dim
    , init
    )

import String.Graphemes
import WChar exposing (stringWidth)


type alias Settings =
    { fontSize : Float
    , lineWidth : Float
    , textWidth : Float
    , textHeight : Float
    , arcRadius : Float
    , color : Colors
    , verbatim :
        { string : String
        , multiline : Bool
        , height : Maybe String
        , width : Maybe String
        }
    }


type alias Colors =
    { stroke : String
    , text : String
    , background : String
    }


type alias Model =
    { rows : Int
    , columns : Int
    , lines : List String
    , settings : Settings
    }


realLength : String -> Int
realLength str =
    str
        |> String.Graphemes.toList
        |> List.map
            (String.toList
                >> List.head
                >> Maybe.map (String.fromChar >> stringWidth >> Maybe.withDefault 0)
                >> Maybe.withDefault 0
            )
        |> List.sum


dim : List String -> ( Int, Int )
dim lines =
    ( List.length lines
    , lines
        |> List.map realLength
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
