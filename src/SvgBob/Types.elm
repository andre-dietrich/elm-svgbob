module SvgBob.Types exposing
    ( Direction(..)
    , Element(..)
    , Position(..)
    , Type(..)
    )


type Position
    = BottomLeftSlantedBottomRight
    | BottomRightSlantedBottomLeft
    | TopRightSlantedTopLeft
    | VerticalTopDownJunctionBottomLeft
    | VerticalTopDownJunctionBottomRight
    | TopLeftSlantedTopRight
    | VerticalTopDownJunctionTopRight
    | VerticalTopDownJunctionTopLeft


type Element
    = Empty
    | Text Char
    | Intersection Type -- also corner
    | Line Direction Direction
    | RoundCorner Position
    | Arrow Direction
    | Curve Float Direction Direction
    | Sequence (List Element)


type Direction
    = East
    | East_ Float
    | South
    | South_ Float
    | North
    | North_ Float
    | West
    | West_ Float
    | Ext Direction Direction



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
