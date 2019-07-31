module SvgBob.Types exposing
    ( Direction(..)
    , Element(..)
    , Point
    , Scan(..)
    , Type(..)
    )

import Array exposing (Array)


type alias Point =
    { x : Float
    , y : Float
    }


type Element
    = Empty
    | Text Char
    | Intersection Type -- also corner
    | Line Direction Direction
    | Arrow Direction
    | Curve Float Direction Direction
    | Sequence (List Element)


type Direction
    = Center
    | East
    | East_ Float
    | South
    | South_ Float
    | North
    | North_ Float
    | West
    | West_ Float
    | Ext Direction Direction
    | Ext_ Float Direction Direction



{--intersection types
--}


type Type
    = Cross
    | HorJunctionTop
    | HorJunctionBot
    | VertJunctionLeft
    | VertJunctionRight
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


type Scan
    = OpenCurve
    | CloseCurve
    | Vertical
    | AlphaNumeric
    | Horizontal
    | LowHorizontal
    | IntersectionX
    | LineX
    | RoundCorner
    | ArrowRight
    | ArrowLeft
    | ArrowDown
    | ArrowUp
    | SlantRight
    | SlantLeft
    | None
