module SvgBob.Model exposing
    ( Colors
    , Model
    , Settings
    , dim
    , init
    , toLines
    )

import String.Graphemes
import WChar exposing (graphemeWidth)


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
    , lines : List (List String)
    , settings : Settings
    }


{-| Lines split into grapheme clusters, so they are only parsed once.
-}
toLines : String -> List (List String)
toLines =
    String.lines >> List.map String.Graphemes.toList


dim : List (List String) -> ( Int, Int )
dim lines =
    ( List.length lines
    , lines
        |> List.map (List.map graphemeWidth >> List.sum)
        |> List.maximum
        |> Maybe.withDefault 0
    )


init : Settings -> String -> Model
init settings str =
    let
        lines =
            toLines str

        ( rows, columns ) =
            dim lines
    in
    { rows = rows
    , columns = columns
    , lines = lines
    , settings = settings
    }
