module SvgBob.Types exposing
    ( Direction(..)
    , Element(..)
    , Point
    , Scan(..)
    )

import Array exposing (Array)


type alias Point =
    { x : Float
    , y : Float
    }


type Element
    = Empty
    | Text String
    | Line Direction Direction
    | Triangle Direction
    | Curve Float Direction Direction
    | Sequence (List Element)
    | Box
    | Circle Bool


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


type Scan
    = OpenCurve
    | CloseCurve
    | Vertical
    | AlphaNumeric
    | Horizontal
    | LowHorizontal
    | Intersection
    | Corner
    | Arrow Direction
    | SlantRight
    | SlantLeft
    | Square
    | None
    | O Bool
