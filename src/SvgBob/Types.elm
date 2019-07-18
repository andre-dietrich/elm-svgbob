module SvgBob.Types exposing
    ( Direction(..)
    , Element(..)
    , Position(..)
    , Type(..)
    )


type Position
    = TopRightCorner
    | TopLeftCorner
    | BottomRightCorner
    | BottomLeftCorner
    | BottomLeftLowHorizontal
    | BottomRightLowHorizontal
    | BottomLeftSlantedTopLeft
    | BottomLeftSlantedTopRight
    | BottomLeftSlantedBottomRight
    | BottomLeftSlantedTopRightLowHorizontal
    | BottomRightSlantedTopRight
    | BottomRightSlantedTopLeftLowHorizontal
    | BottomRightSlantedTopLeft
    | BottomRightSlantedBottomLeft
    | TopLeftSlantedBottomLeft
    | TopLeftSlantedBottomRight
    | TopRightSlantedBottomRight
    | TopRightSlantedBottomLeft
    | TopRightSlantedTopLeft
    | SlantedRightJunctionRight
    | SlantedLeftJunctionLeft
    | SlantedRightJunctionLeft
    | SlantedLeftJunctionRight
    | VerticalTopDownJunctionBottomLeft
    | VerticalTopDownJunctionBottomRight
    | TopLeftSlantedTopRight
    | VerticalTopDownJunctionTopRight
    | VerticalTopDownJunctionTopLeft
    | TopLeftBigCurve
    | TopRightBigCurve
    | BottomLeftBigCurve
    | BottomRightBigCurve


type Element
    = Empty
    | Intersection Type -- also corner
    | Horizontal
    | LowHorizontal
    | LowHorizontalExtendLeft
    | LowHorizontalExtendVerticalLeft
    | LowHorizontalExtendRight
    | LowHorizontalExtendVerticalRight
    | LowHorizontalExtendVerticalBottomLeft
    | LowHorizontalExtendVerticalBottomRight
    | Vertical
    | RoundCorner Position
    | Arrow Direction
    | SlantRight
    | SlantLeft
    | OpenCurve
    | CloseCurve
    | BigOpenCurve
    | BigCloseCurve
    | Text Char


type Direction
    = East
    | South
    | SouthWest
    | SouthEast
    | North
    | NorthWest
    | NorthEast
    | West



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
