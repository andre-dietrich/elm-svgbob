module SvgBob.Types exposing (Element(..), Position(..), Type(..))


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
    = Intersection Type -- also corner
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
    | ArrowEast
    | ArrowSouth
    | ArrowSouthWest
    | ArrowSouthEast
    | ArrowNorth
    | ArrowNorthWest
    | ArrowNorthEast
    | ArrowWest
    | SlantRight
    | SlantLeft
    | OpenCurve
    | CloseCurve
    | BigOpenCurve
    | BigCloseCurve
    | Text Char



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
