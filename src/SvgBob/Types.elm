module SvgBob.Types exposing
    ( Direction(..)
    , Element(..)
    , Point
    , Scan(..)
    , isVerbatim
    , mergeVerbatim
    )


type alias Point =
    { x : Float
    , y : Float
    }


type Element
    = Empty
    | Text Char
    | Line Direction Direction
    | Triangle Direction
    | Curve Float Direction Direction
    | Sequence (List Element)
    | Box
    | Circle Bool
    | ForeignObject String


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
    | Verbatim String
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


isVerbatim : Scan -> Bool
isVerbatim v =
    case v of
        Verbatim _ ->
            True

        _ ->
            False


mergeVerbatim : Scan -> Scan -> Scan
mergeVerbatim scan scan2 =
    case ( scan, scan2 ) of
        ( Verbatim str, Verbatim str2 ) ->
            Verbatim (str ++ "\n" ++ str2)

        _ ->
            scan
