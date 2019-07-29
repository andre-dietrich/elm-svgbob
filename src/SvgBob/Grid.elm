module SvgBob.Grid exposing (getSvg)

import Array exposing (Array)
import Char
import Color
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute)
import String
import Svg exposing (Svg)
import Svg.Attributes as Attr
import SvgBob.Model exposing (Model, Settings)
import SvgBob.Types
    exposing
        ( Direction(..)
        , Element(..)
        , Point
        , Type(..)
        )


move : Direction -> Point -> Point
move dir pt =
    case dir of
        South ->
            { pt | y = pt.y + textHeight / 2 }

        South_ n ->
            { pt | y = pt.y + textHeight / 2 * n }

        North ->
            { pt | y = pt.y - textHeight / 2 }

        North_ n ->
            { pt | y = pt.y - textHeight / 2 * n }

        East ->
            { pt | x = pt.x + textWidth / 2 }

        East_ n ->
            { pt | x = pt.x + textWidth / 2 * n }

        West ->
            { pt | x = pt.x - textWidth / 2 }

        West_ n ->
            { pt | x = pt.x - textWidth / 2 * n }

        Ext dir1 dir2 ->
            pt
                |> move dir1
                |> move dir2

        Ext_ n dir1 dir2 ->
            pt
                |> move (moveExt n dir1)
                |> move (moveExt n dir2)


moveExt : Float -> Direction -> Direction
moveExt n dir =
    case dir of
        South ->
            South_ n

        South_ m ->
            South_ (n * m)

        North ->
            North_ n

        North_ m ->
            North_ (n * m)

        East ->
            East_ n

        East_ m ->
            East_ (n * m)

        West ->
            West_ n

        West_ m ->
            West_ (n * m)

        Ext dir1 dir2 ->
            Ext (moveExt n dir1) (moveExt n dir2)

        Ext_ m dir1 dir2 ->
            Ext (moveExt (n * m) dir1) (moveExt (n * m) dir2)


type alias Matrix =
    { north_west : Maybe Char
    , north : Maybe Char
    , north_east : Maybe Char
    , west : Maybe Char
    , center : Char
    , east : Maybe Char
    , south_west : Maybe Char
    , south : Maybe Char
    , south_east : Maybe Char
    }


getMatrix : Int -> Int -> Array (Array Char) -> Maybe Matrix
getMatrix x y lines =
    case get x y lines of
        Just center ->
            Just
                { north_west = get (x - 1) (y - 1) lines
                , north = get x (y - 1) lines
                , north_east = get (x + 1) (y - 1) lines
                , west = get (x - 1) y lines
                , center = center
                , east = get (x + 1) y lines
                , south_west = get (x - 1) (y + 1) lines
                , south = get x (y + 1) lines
                , south_east = get (x + 1) (y + 1) lines
                }

        _ ->
            Nothing


textWidth : Float
textWidth =
    8.0


textHeight : Float
textHeight =
    16.0


vertical : List Char
vertical =
    [ '|' ]


verticalDashed : List Char
verticalDashed =
    [ ':' ]


horizontal : List Char
horizontal =
    [ '-' ]


horizontalDouble : List Char
horizontalDouble =
    [ '=' ]


lowHorizontal : List Char
lowHorizontal =
    [ '_' ]


intersections : List Char
intersections =
    [ '+' ]


roundCorners : List Char
roundCorners =
    [ '.', '\'' ]


arrowRight : List Char
arrowRight =
    [ '>' ]


arrowDown : List Char
arrowDown =
    [ 'V', 'v' ]


arrowLeft : List Char
arrowLeft =
    [ '<' ]


arrowUp : List Char
arrowUp =
    [ '^', 'î' ]


slantRight : List Char
slantRight =
    [ '/' ]


slantLeft : List Char
slantLeft =
    [ '\\' ]


openCurve : List Char
openCurve =
    [ '(' ]


closeCurve : List Char
closeCurve =
    [ ')' ]


member : List Char -> Char -> Bool
member list char =
    List.member char list


isOpenCurve : Char -> Bool
isOpenCurve =
    member openCurve



--close parenthesis


isCloseCurve : Char -> Bool
isCloseCurve =
    member closeCurve


isVertical : Char -> Bool
isVertical =
    member vertical


isAlphaNumeric : Char -> Bool
isAlphaNumeric char =
    Char.isDigit char || Char.isUpper char || Char.isLower char


isHorizontal : Char -> Bool
isHorizontal =
    member horizontal


isLowHorizontal : Char -> Bool
isLowHorizontal =
    member lowHorizontal


isIntersection : Char -> Bool
isIntersection =
    member intersections


isLine : Char -> Bool
isLine char =
    isVertical char || isHorizontal char || isLowHorizontal char


isRoundCorner : Char -> Bool
isRoundCorner =
    member roundCorners


isArrowRight : Char -> Bool
isArrowRight =
    member arrowRight


isArrowLeft : Char -> Bool
isArrowLeft =
    member arrowLeft


isArrowDown : Char -> Bool
isArrowDown =
    member arrowDown


isArrowUp : Char -> Bool
isArrowUp =
    member arrowUp


isSlantRight : Char -> Bool
isSlantRight =
    member slantRight


isSlantLeft : Char -> Bool
isSlantLeft =
    member slantLeft


leftOf : Int -> Int -> Array (Array c) -> Maybe c
leftOf x y =
    get (x - 1) y


rightOf : Int -> Int -> Array (Array c) -> Maybe c
rightOf x y =
    get (x + 1) y


topOf : Int -> Int -> Array (Array c) -> Maybe c
topOf x y =
    get x (y - 1)


bottomOf : Int -> Int -> Array (Array c) -> Maybe c
bottomOf x y =
    get x (y + 1)


topLeftOf : Int -> Int -> Array (Array c) -> Maybe c
topLeftOf x y =
    get (x - 1) (y - 1)


topRightOf : Int -> Int -> Array (Array c) -> Maybe c
topRightOf x y =
    get (x + 1) (y - 1)


bottomLeftOf : Int -> Int -> Array (Array c) -> Maybe c
bottomLeftOf x y =
    get (x - 1) (y + 1)


bottomRightOf : Int -> Int -> Array (Array c) -> Maybe c
bottomRightOf x y =
    get (x + 1) (y + 1)


isNeighbor : Maybe c -> (c -> Bool) -> Bool
isNeighbor neighbor check =
    case neighbor of
        Just neighbor_ ->
            check neighbor_

        Nothing ->
            False


lowHorizontalLine : Matrix -> Element
lowHorizontalLine { center, west, east, south_west, south_east } =
    if isNeighbor west isSlantRight then
        Line (Ext South East) (West_ 4)

    else if isNeighbor west isVertical then
        Line (Ext South East) (West_ 3)

    else if isNeighbor east isSlantLeft then
        Line (Ext South West) (East_ 4)

    else if isNeighbor east isVertical then
        Line (Ext South West) (East_ 3)

    else if isNeighbor south_west isVertical then
        Line (Ext South (West_ 2)) (East_ 3)

    else if isNeighbor south_east isVertical then
        Line (Ext South West) (East_ 3)

    else if
        not (isNeighbor west isAlphaNumeric)
            && not (isNeighbor east isAlphaNumeric)
    then
        Line (Ext South East) (West_ 2)

    else
        Text center


intersection : Matrix -> Element
intersection { center, south, west, north, east, south_west, south_east } =
    let
        isVerticalJunctionLeft =
            isNeighbor north isVertical
                && isNeighbor south isVertical
                && isNeighbor west isHorizontal

        isVerticalJunctionRight =
            isNeighbor north isVertical
                && isNeighbor south isVertical
                && isNeighbor east isHorizontal

        isHorizontalJunctionTop =
            isNeighbor west isHorizontal
                && isNeighbor east isHorizontal
                && isNeighbor north isVertical

        isHorizontalJunctionBot =
            isNeighbor west isHorizontal
                && isNeighbor east isHorizontal
                && isNeighbor south isVertical

        isTopLeftIntersection =
            isNeighbor south isVertical && isNeighbor east isHorizontal

        isTopRightIntersection =
            isNeighbor south isVertical && isNeighbor west isHorizontal

        isBottomRightIntersection =
            isNeighbor north isVertical && isNeighbor west isHorizontal

        isBottomLeftIntersection =
            isNeighbor north isVertical && isNeighbor east isHorizontal

        isCrossIntersection =
            isNeighbor north isVertical
                && isNeighbor south isVertical
                && isNeighbor west isHorizontal
                && isNeighbor east isHorizontal
    in
    if isCrossIntersection then
        Intersection Cross

    else if isVerticalJunctionLeft then
        Intersection VertJunctionLeft

    else if isVerticalJunctionRight then
        Intersection VertJunctionRight

    else if isHorizontalJunctionTop then
        Intersection HorJunctionTop

    else if isHorizontalJunctionBot then
        Intersection HorJunctionBot

    else if isTopRightIntersection then
        Intersection TopRight

    else if isTopLeftIntersection then
        Intersection TopLeft

    else if isBottomRightIntersection then
        Intersection BottomRight

    else if isBottomLeftIntersection then
        Intersection BottomLeft

    else
        Empty


getElement : Int -> Int -> Model -> Element
getElement x y model =
    let
        lines =
            model.lines

        top =
            topOf x y lines

        bottom =
            bottomOf x y lines

        left =
            leftOf x y lines

        right =
            rightOf x y lines

        topLeft =
            topLeftOf x y lines

        topRight =
            topRightOf x y lines

        bottomLeft =
            bottomLeftOf x y lines

        bottomRight =
            bottomRightOf x y lines
    in
    case getMatrix x y model.lines of
        Nothing ->
            Empty

        Just m ->
            if
                isVertical m.center
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Line South (Ext North North)

            else if
                isHorizontal m.center
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Line East (West_ 2)

            else if isLowHorizontal m.center then
                lowHorizontalLine m

            else if isIntersection m.center then
                intersection m

            else if isRoundCorner m.center then
                if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor right isHorizontal
                then
                    -- RoundCorner SlantedRightJunctionRight
                    Sequence
                        [ Curve 2
                            East
                            (Ext (South_ 0.5) (West_ 1.5))
                        , Line
                            (Ext North East)
                            (Ext_ 2 South West)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft
                then
                    -- RoundCorner VerticalTopDownJunctionTopLeft
                    Sequence
                        [ Curve 4
                            (South_ 0.5)
                            (Ext North (West_ 0.5))
                        , Line South (North_ 2)
                        , Line
                            (Ext North West)
                            (Ext_ 0.5 South East)
                        ]

                else if
                    isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor left isHorizontal
                then
                    -- RoundCorner SlantedLeftJunctionLeft
                    Sequence
                        [ Curve 2
                            (Ext_ 0.5 South East)
                            (Ext (North_ 0.5) (West_ 1.5))
                        , Line (Ext North West) (Ext_ 2 South East)
                        ]

                else if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor left isHorizontal
                then
                    -- RoundCorner SlantedRightJunctionLeft XXX
                    Sequence
                        [ Curve 2
                            West
                            (Ext (North_ 0.5) (East_ 1.5))
                        , Line
                            (Ext North East)
                            (Ext_ 2 South West)
                        ]

                else if
                    isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor right isHorizontal
                then
                    -- RoundCorner SlantedLeftJunctionRight
                    Sequence
                        [ Curve 2
                            (Ext (North_ 0.5) (West_ 0.5))
                            (Ext (South_ 0.5) (East_ 1.5))
                        , Line
                            (Ext North West)
                            (Ext (South_ 2) (East_ 2))
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomLeft isSlantRight
                then
                    -- RoundCorner VerticalTopDownJunctionBottomLeft
                    Sequence
                        [ Curve 4
                            (Ext_ 0.5 South West)
                            (Ext North (East_ 0.5))
                        , Line South (North_ 2)
                        , Line
                            (Ext South West)
                            (Ext_ 0.5 North East)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomRight isSlantLeft
                then
                    -- RoundCorner VerticalTopDownJunctionBottomRight
                    Sequence
                        [ Curve 4
                            (North_ 0.5)
                            (Ext South (East_ 0.5))
                        , Line South (North_ 2)
                        , Line
                            (Ext South East)
                            (Ext_ 0.5 North West)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight
                then
                    -- RoundCorner VerticalTopDownJunctionTopRight
                    Sequence
                        [ Curve 4
                            (Ext_ 0.5 North East)
                            (Ext South (West_ 0.5))
                        , Line South (North_ 2)
                        , Line
                            (Ext North East)
                            (Ext_ 0.5 South West)
                        ]

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor right isHorizontal
                then
                    Sequence
                        [ Curve 1
                            East
                            (Ext (South_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor left isHorizontal
                then
                    Sequence
                        [ Curve 1
                            (South_ 0.5)
                            (Ext (North_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight
                then
                    -- RoundCorner TopLeftSlantedTopRight
                    Sequence
                        [ Curve 4
                            (Ext_ 0.5 North East)
                            (Ext South (West_ 0.5))
                        , Line South (North_ 0.5)
                        , Line
                            (Ext North East)
                            (Ext_ 0.5 South West)
                        ]

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isOpenCurve
                then
                    Curve 4
                        East
                        (Ext South (West_ 3))

                else if
                    isNeighbor right isRoundCorner
                        && isNeighbor bottomLeft isOpenCurve
                then
                    Curve 4
                        East
                        (Ext South (West_ 3))

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomRight isCloseCurve
                then
                    Curve 4
                        (Ext South (East_ 2))
                        (Ext North (West_ 3))

                else if
                    isNeighbor left isRoundCorner
                        && isNeighbor bottomRight isCloseCurve
                then
                    Curve 4
                        (Ext South (East_ 2))
                        (Ext North (West_ 3))

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topLeft isOpenCurve
                then
                    Curve 4
                        (Ext North (West_ 2))
                        (Ext South (East_ 3))

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topRight isCloseCurve
                then
                    Curve 4
                        West
                        (Ext North (East_ 3))

                else if
                    isNeighbor right isRoundCorner
                        && isNeighbor topLeft isOpenCurve
                then
                    Curve 4
                        (Ext North (West_ 2))
                        (Ext South (East_ 3))

                else if
                    isNeighbor left isRoundCorner
                        && isNeighbor topRight isCloseCurve
                then
                    Curve 4
                        West
                        (Ext North (East_ 3))

                else if
                    isNeighbor top isVertical
                        && isNeighbor right isHorizontal
                then
                    Sequence
                        [ Curve 1
                            (North_ 0.5)
                            (Ext (South_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor right isLowHorizontal
                then
                    Sequence
                        [ Curve 1
                            (South_ 0.5)
                            (Ext (South_ 0.5) East)
                        , Line North (South_ 1.5)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor left isLowHorizontal
                then
                    Sequence
                        [ Curve 1
                            (Ext South West)
                            (Ext (North_ 0.5) East)
                        , Line North (South_ 1.5)
                        ]

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    Sequence
                        [ Curve 2
                            (Ext_ 0.5 North West)
                            (Ext (South_ 0.5) (East_ 1.5))
                        , Line
                            (Ext North West)
                            (Ext_ 0.5 South East)
                        ]

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    -- RoundCorner BottomLeftSlantedTopRight
                    Sequence
                        [ Curve 1
                            (Ext_ 0.5 North East)
                            (Ext_ 0.5 South East)
                        , Line
                            (Ext North East)
                            (Ext_ 0.5 South West)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottomRight isSlantLeft
                then
                    -- RoundCorner BottomLeftSlantedBottomRight
                    Sequence
                        [ Curve 4
                            (North_ 0.5)
                            (Ext South (East_ 0.5))
                        , Line North (South_ 0.5)
                        , Line
                            (Ext South East)
                            (Ext_ 0.5 North West)
                        ]

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    Sequence
                        [ Curve 2
                            West
                            (Ext (North_ 0.5) (East_ 1.5))
                        , Line
                            (Ext_ 0.5 North East)
                            (Ext_ 0.5 North East)
                        ]

                else if
                    isNeighbor right isLowHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    -- RoundCorner BottomLeftSlantedTopRightLowHorizontal
                    Sequence
                        [ Curve 1.5
                            (East_ 0)
                            (Ext South East)
                        , Line (Ext North East) (Ext South West)
                        ]

                else if
                    isNeighbor left isLowHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    -- RoundCorner BottomRightSlantedTopLeftLowHorizontal
                    Sequence
                        [ Curve 1.5
                            (Ext South West)
                            (Ext North East)
                        , Line
                            (Ext North West)
                            (Ext South East)
                        ]

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    Sequence
                        [ Curve 1
                            West
                            (Ext_ 0.5 North East)
                        , Line
                            (Ext North West)
                            (Ext_ 0.5 South East)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottomLeft isSlantRight
                then
                    --  RoundCorner BottomRightSlantedBottomLeft
                    Sequence
                        [ Curve 4
                            (Ext_ 0.5 South West)
                            (Ext North (East_ 0.5))
                        , Line North (South_ 0.5)
                        , Line
                            (Ext South West)
                            (Ext_ 0.5 North East)
                        ]

                else if
                    isNeighbor top isVertical
                        && isNeighbor left isHorizontal
                then
                    Sequence
                        [ Curve 1
                            West
                            (Ext (North_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottom isRoundCorner
                then
                    Sequence
                        [ Curve 1
                            East
                            (Ext (South_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottom isRoundCorner
                then
                    Sequence
                        [ Curve 1
                            (South_ 0.5)
                            (Ext (North_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor top isRoundCorner
                then
                    Sequence
                        [ Curve 1
                            West
                            (Ext (North_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor top isRoundCorner
                then
                    Sequence
                        [ Curve 1
                            (North_ 0.5)
                            (Ext (South_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isSlantRight
                then
                    Sequence
                        [ Curve 2
                            East
                            (Ext (South_ 0.5) (West_ 1.5))
                        , Line (Ext South West)
                            (Ext (North_ 0.5) (East_ 0.5))
                        ]

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomRight isSlantLeft
                then
                    Sequence
                        [ Curve 1
                            East
                            (Ext (South_ 0.5) (West_ 0.5))
                        , Line
                            (Ext South East)
                            (Ext_ 0.5 North West)
                        ]

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomRight isSlantLeft
                then
                    Sequence
                        [ Curve 2
                            (Ext_ 0.5 South East)
                            (Ext (North_ 0.5) (West_ 1.5))
                        , Line
                            (Ext South East)
                            (Ext_ 0.5 North West)
                        ]

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomLeft isSlantRight
                then
                    --Todo
                    Sequence
                        [ Curve 1
                            (Ext_ 0.5 South West)
                            (Ext_ 0.5 North West)
                        , Line
                            (Ext South West)
                            (Ext_ 0.5 North East)
                        ]

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft
                then
                    -- RoundCorner TopRightSlantedTopLeft
                    Sequence
                        [ Curve 4
                            (South_ 0.5)
                            (Ext North (West_ 0.5))
                        , Line South (North_ 0.5)
                        , Line
                            (Ext North West)
                            (Ext_ 0.5 South East)
                        ]

                else
                    Text m.center

            else if isArrowRight m.center then
                Arrow West

            else if isArrowDown m.center then
                if isNeighbor top isVertical then
                    Arrow North

                else if isNeighbor topRight isSlantRight then
                    Arrow <| Ext North East

                else if isNeighbor topLeft isSlantLeft then
                    Arrow <| Ext North West

                else
                    Text m.center

            else if isArrowLeft m.center then
                Arrow East

            else if isArrowUp m.center then
                if isNeighbor bottom isVertical then
                    Arrow South

                else if isNeighbor bottomLeft isSlantRight then
                    Arrow <| Ext South West

                else if isNeighbor bottomRight isSlantLeft then
                    Arrow <| Ext South East

                else
                    Text m.center

            else if isSlantRight m.center then
                Line
                    (Ext North East)
                    (Ext_ 2 South West)

            else if isSlantLeft m.center then
                Line
                    (Ext South East)
                    (Ext_ 2 North West)

            else if isOpenCurve m.center then
                if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomRight isSlantLeft
                then
                    Curve 4
                        (Ext North East)
                        (Ext South South)

                else if
                    isNeighbor topRight isRoundCorner
                        && isNeighbor bottomRight isRoundCorner
                then
                    Curve 4
                        North
                        (Ext South South)

                else
                    Text m.center

            else if
                isCloseCurve m.center
                    && isNeighbor topLeft isRoundCorner
                    && isNeighbor bottomLeft isRoundCorner
            then
                Curve 4
                    South
                    (Ext North North)

            else if
                isCloseCurve m.center
                    && isNeighbor topLeft isSlantLeft
                    && isNeighbor bottomLeft isSlantRight
            then
                Curve 4
                    (Ext South West)
                    (Ext North North)

            else if m.center /= ' ' then
                Text m.center

            else
                Empty


vectorEffect : Attribute a
vectorEffect =
    attribute "vector-effect" "none"


drawArc : Settings -> Float -> Point -> Direction -> Svg a
drawArc s faktor pos dir =
    let
        pos2 =
            move dir pos

        radius =
            s.arcRadius * faktor
    in
    Svg.path
        [ [ "M"
          , String.fromFloat pos.x
          , String.fromFloat pos.y
          , "A"
          , String.fromFloat radius
          , String.fromFloat radius
          , "0"
          , "0"
          , "0"
          , String.fromFloat pos2.x
          , String.fromFloat pos2.y
          ]
            |> String.join " "
            |> Attr.d
        , Attr.stroke "black"
        , Attr.strokeWidth <| String.fromFloat s.lineWidth
        , Attr.fill "transparent"
        , vectorEffect
        ]
        []


arrowMarker : Svg a
arrowMarker =
    Svg.marker
        [ Attr.id "triangle"
        , Attr.viewBox "0 0 14 14"
        , Attr.refX "0"
        , Attr.refY "5"
        , Attr.markerUnits "strokeWidth"
        , Attr.markerWidth "10"
        , Attr.markerHeight "10"
        , Attr.orient "auto"
        ]
        [ Svg.path [ Attr.d "M 0 0 L 10 5 L 0 10 z", vectorEffect ]
            []
        ]


getSvg : List (Svg.Attribute msg) -> Model -> Html msg
getSvg attr model =
    let
        gwidth =
            String.fromFloat <| measureX model.columns + 10

        gheight =
            String.fromFloat <| measureY model.rows + 10
    in
    Svg.svg (Attr.viewBox ("0 0 " ++ gwidth ++ " " ++ gheight) :: attr)
        (Svg.defs []
            [ arrowMarker ]
            :: drawPaths model
        )



--TODO: optimize here to indexedMap only the lines and chars
--TODO: modularized parts in order to easily fit and match


drawPaths : Model -> List (Svg a)
drawPaths model =
    model.lines
        |> Array.indexedMap
            (\r line ->
                Array.indexedMap
                    (\c _ ->
                        drawElement c r model
                    )
                    line
                    |> Array.toList
            )
        |> Array.toList
        |> List.concat
        |> List.concat


drawElement : Int -> Int -> Model -> List (Svg a)
drawElement x y model =
    let
        position =
            Point
                (measureX x + textWidth / 2)
                (measureY y + textHeight / 2)
    in
    case getElement x y model of
        Intersection itype ->
            drawIntersection x y itype model

        element ->
            draw model.settings position element


draw : Settings -> Point -> Element -> List (Svg a)
draw settings pos element =
    case element of
        Arrow dir ->
            [ drawArrow settings pos dir ]

        Text char ->
            [ drawText settings pos char ]

        Line start stop ->
            [ drawLineX settings (move start pos) stop ]

        Curve faktor start stop ->
            [ drawArc settings faktor (move start pos) stop ]

        Sequence elements ->
            elements
                |> List.map (draw settings pos)
                |> List.concat

        _ ->
            []


opposite : Direction -> Direction
opposite dir =
    case dir of
        East ->
            West

        East_ n ->
            West_ n

        West ->
            East

        West_ n ->
            East_ n

        North ->
            South

        North_ n ->
            South_ n

        South ->
            North

        South_ n ->
            North_ n

        Ext dir1 dir2 ->
            Ext (opposite dir1) (opposite dir2)

        Ext_ n dir1 dir2 ->
            Ext_ n (opposite dir1) (opposite dir2)


drawIntersection : Int -> Int -> Type -> Model -> List (Svg a)
drawIntersection x y itype model =
    let
        --vertical line
        v1startX =
            measureX x + textWidth / 2

        v1endX =
            v1startX

        v1startY =
            measureY y

        v1endY =
            measureY y + textHeight / 2

        -- v line part 2
        v2startX =
            measureX x + textWidth / 2

        v2endX =
            v2startX

        v2startY =
            measureY y + textHeight / 2

        v2endY =
            measureY y + textHeight

        --horizontal line
        h1startX =
            measureX x

        h1endX =
            measureX x + textWidth / 2

        h1startY =
            measureY y + textHeight / 2

        h1endY =
            h1startY

        --h line part 2
        h2startX =
            measureX x + textWidth / 2

        h2endX =
            h2startX + textWidth

        h2startY =
            measureY y + textHeight / 2

        h2endY =
            h2startY

        v1Line =
            drawLine v1startX v1startY v1endX v1endY model.settings

        v2Line =
            drawLine v2startX v2startY v2endX v2endY model.settings

        h1Line =
            drawLine h1startX h1startY h1endX h1endY model.settings

        h2Line =
            drawLine h2startX h2startY h2endX h2endY model.settings
    in
    case itype of
        VertJunctionLeft ->
            [ v1Line, v2Line, h1Line ]

        VertJunctionRight ->
            [ v1Line, v2Line, h2Line ]

        HorJunctionTop ->
            [ h1Line, h2Line, v1Line ]

        HorJunctionBot ->
            [ h1Line, h2Line, v2Line ]

        TopLeft ->
            [ h2Line, v2Line ]

        TopRight ->
            [ h1Line, v2Line ]

        BottomLeft ->
            [ v1Line, h2Line ]

        BottomRight ->
            [ v1Line, h1Line ]

        Cross ->
            [ v1Line, v2Line, h1Line, h2Line ]


colorText : Color.Color -> String
colorText color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    "rgb("
        ++ String.fromFloat red
        ++ ","
        ++ String.fromFloat green
        ++ ","
        ++ String.fromFloat blue
        ++ ")"


drawArrow : Settings -> Point -> Direction -> Svg a
drawArrow settings pos dir =
    toLine
        [ Attr.style
            ("stroke: "
                ++ colorText settings.color
                ++ ";stroke-width:"
                ++ String.fromFloat settings.lineWidth
            )
        , Attr.markerEnd "url(#triangle)"
        , vectorEffect
        ]
        (move dir pos)
        (opposite dir)


drawLine : Float -> Float -> Float -> Float -> Settings -> Svg a
drawLine startX startY endX endY s =
    Svg.line
        [ Attr.x1 <| String.fromFloat startX
        , Attr.x2 <| String.fromFloat endX
        , Attr.y1 <| String.fromFloat startY
        , Attr.y2 <| String.fromFloat endY
        , Attr.stroke <| colorText s.color
        , Attr.strokeWidth <| String.fromFloat s.lineWidth
        , Attr.strokeLinecap "round"
        , Attr.strokeLinejoin "mitter"
        , vectorEffect
        ]
        []


toLine : List (Svg.Attribute msg) -> Point -> Direction -> Svg msg
toLine misc pos dir =
    let
        pos2 =
            move dir pos
    in
    Svg.line
        (List.append
            misc
            [ Attr.x1 <| String.fromFloat pos.x
            , Attr.x2 <| String.fromFloat pos2.x
            , Attr.y1 <| String.fromFloat pos.y
            , Attr.y2 <| String.fromFloat pos2.y
            ]
        )
        []


drawLineX : Settings -> Point -> Direction -> Svg a
drawLineX s =
    toLine
        [ Attr.stroke <| colorText s.color
        , Attr.strokeWidth <| String.fromFloat s.lineWidth
        , Attr.strokeLinecap "round"
        , Attr.strokeLinejoin "mitter"
        , vectorEffect
        ]


drawText : Settings -> Point -> Char -> Svg a
drawText s pos char =
    let
        pos2 =
            move (Ext (South_ 0.5) West) pos
    in
    Svg.node "text"
        [ Attr.x <| String.fromFloat pos2.x
        , Attr.y <| String.fromFloat pos2.y
        , Attr.style
            ("font-size:"
                ++ String.fromFloat s.fontSize
                ++ "px;font-family:monospace"
            )
        ]
        [ Svg.text <| String.fromChar char ]


measureX : Int -> Float
measureX x =
    toFloat x * textWidth


measureY : Int -> Float
measureY y =
    toFloat y * textHeight


get : Int -> Int -> Array (Array c) -> Maybe c
get x y lines =
    lines
        |> Array.get y
        |> Maybe.map (Array.get x)
        |> Maybe.withDefault Nothing
