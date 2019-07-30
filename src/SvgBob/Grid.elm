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


verticalDashed : List Char
verticalDashed =
    [ ':' ]


horizontalDouble : List Char
horizontalDouble =
    [ '=' ]


member : List Char -> Maybe Char -> Bool
member list char =
    case char of
        Just c ->
            List.member c list

        Nothing ->
            False


isOpenCurve : Maybe Char -> Bool
isOpenCurve =
    member [ '(' ]


isCloseCurve : Maybe Char -> Bool
isCloseCurve =
    member [ ')' ]


isVertical : Maybe Char -> Bool
isVertical =
    member [ '|' ]


isAlphaNumeric : Maybe Char -> Bool
isAlphaNumeric char =
    case char of
        Just c ->
            Char.isDigit c || Char.isUpper c || Char.isLower c

        _ ->
            False


isHorizontal : Maybe Char -> Bool
isHorizontal =
    member [ '-' ]


isLowHorizontal : Maybe Char -> Bool
isLowHorizontal =
    member [ '_' ]


isIntersection : Maybe Char -> Bool
isIntersection =
    member [ '+' ]


isLine : Maybe Char -> Bool
isLine char =
    isVertical char || isHorizontal char || isLowHorizontal char


isRoundCorner : Maybe Char -> Bool
isRoundCorner =
    member [ '.', '\'' ]


isArrowRight : Maybe Char -> Bool
isArrowRight =
    member [ '>' ]


isArrowLeft : Maybe Char -> Bool
isArrowLeft =
    member [ '<' ]


isArrowDown : Maybe Char -> Bool
isArrowDown =
    member [ 'V', 'v' ]


isArrowUp : Maybe Char -> Bool
isArrowUp =
    member [ '^', 'î' ]


isSlantRight : Maybe Char -> Bool
isSlantRight =
    member [ '/' ]


isSlantLeft : Maybe Char -> Bool
isSlantLeft =
    member [ '\\' ]


lowHorizontalLine : Matrix -> Element
lowHorizontalLine { center, west, east, south_west, south_east } =
    if isSlantRight west then
        Line (Ext South East) (West_ 4)

    else if isVertical west then
        Line (Ext South East) (West_ 3)

    else if isSlantLeft east then
        Line (Ext South West) (East_ 4)

    else if isVertical east then
        Line (Ext South West) (East_ 3)

    else if isVertical south_west then
        Line (Ext South (West_ 2)) (East_ 3)

    else if isVertical south_east then
        Line (Ext South West) (East_ 3)

    else if not (isAlphaNumeric west) && not (isAlphaNumeric east) then
        Line (Ext South East) (West_ 2)

    else
        Text center


intersection : Matrix -> Element
intersection { center, south, west, north, east, south_west, south_east } =
    let
        isVerticalJunctionLeft =
            isVertical north && isVertical south && isHorizontal west

        isVerticalJunctionRight =
            isVertical north
                && isVertical south
                && isHorizontal east

        isHorizontalJunctionTop =
            isHorizontal west
                && isHorizontal east
                && isVertical north

        isHorizontalJunctionBot =
            isHorizontal west
                && isHorizontal east
                && isVertical south

        isTopLeftIntersection =
            isVertical south && isHorizontal east

        isTopRightIntersection =
            isVertical south && isHorizontal west

        isBottomRightIntersection =
            isVertical north && isHorizontal west

        isBottomLeftIntersection =
            isVertical north && isHorizontal east

        isCrossIntersection =
            isVertical north
                && isVertical south
                && isHorizontal west
                && isHorizontal east
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
    case getMatrix x y model.lines of
        Nothing ->
            Empty

        Just m ->
            let
                center =
                    Just m.center
            in
            if
                isVertical center
                    && not (isAlphaNumeric m.west)
                    && not (isAlphaNumeric m.east)
            then
                Line South (Ext North North)

            else if
                isHorizontal center
                    && not (isAlphaNumeric m.west)
                    && not (isAlphaNumeric m.east)
            then
                Line East (West_ 2)

            else if isLowHorizontal center then
                lowHorizontalLine m

            else if isIntersection center then
                intersection m

            else if isRoundCorner center then
                if
                    isSlantRight m.north_east
                        && isSlantRight m.south_west
                        && isHorizontal m.east
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
                    isVertical m.north
                        && isVertical m.south
                        && isSlantLeft m.north_west
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
                    isSlantLeft m.north_west
                        && isSlantLeft m.south_east
                        && isHorizontal m.west
                then
                    -- RoundCorner SlantedLeftJunctionLeft
                    Sequence
                        [ Curve 2
                            (Ext_ 0.5 South East)
                            (Ext (North_ 0.5) (West_ 1.5))
                        , Line (Ext North West) (Ext_ 2 South East)
                        ]

                else if
                    isSlantRight m.north_east
                        && isSlantRight m.south_west
                        && isHorizontal m.west
                then
                    -- RoundCorner SlantedRightJunctionLeft
                    Sequence
                        [ Curve 2
                            West
                            (Ext (North_ 0.5) (East_ 1.5))
                        , Line
                            (Ext North East)
                            (Ext_ 2 South West)
                        ]

                else if
                    isSlantLeft m.north_west
                        && isSlantLeft m.south_east
                        && isHorizontal m.east
                then
                    -- RoundCorner SlantedLeftJunctionRight
                    Sequence
                        [ Curve 2
                            (Ext_ 0.5 North West)
                            (Ext (South_ 0.5) (East_ 1.5))
                        , Line
                            (Ext North West)
                            (Ext_ 2 South East)
                        ]

                else if
                    isVertical m.north
                        && isVertical m.south
                        && isSlantRight m.south_west
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
                    isVertical m.north
                        && isVertical m.south
                        && isSlantLeft m.south_east
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
                    isVertical m.north
                        && isVertical m.south
                        && isSlantRight m.north_east
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

                else if isVertical m.south && isHorizontal m.east then
                    Sequence
                        [ Curve 1
                            East
                            (Ext (South_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if isVertical m.south && isHorizontal m.west then
                    Sequence
                        [ Curve 1
                            (South_ 0.5)
                            (Ext (North_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if isVertical m.south && isSlantRight m.north_east then
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

                else if isHorizontal m.east && isOpenCurve m.south_west then
                    Curve 4
                        East
                        (Ext South (West_ 3))

                else if isRoundCorner m.east && isOpenCurve m.south_west then
                    Curve 4
                        East
                        (Ext South (West_ 3))

                else if isHorizontal m.west && isCloseCurve m.south_east then
                    Curve 4
                        (Ext South (East_ 2))
                        (Ext North (West_ 3))

                else if isRoundCorner m.west && isCloseCurve m.south_east then
                    Curve 4
                        (Ext South (East_ 2))
                        (Ext North (West_ 3))

                else if isHorizontal m.east && isOpenCurve m.north_west then
                    Curve 4
                        (Ext North (West_ 2))
                        (Ext South (East_ 3))

                else if isHorizontal m.west && isCloseCurve m.north_east then
                    Curve 4
                        West
                        (Ext North (East_ 3))

                else if isRoundCorner m.east && isOpenCurve m.north_west then
                    Curve 4
                        (Ext North (West_ 2))
                        (Ext South (East_ 3))

                else if isRoundCorner m.west && isCloseCurve m.north_east then
                    Curve 4
                        West
                        (Ext North (East_ 3))

                else if isVertical m.north && isHorizontal m.east then
                    Sequence
                        [ Curve 1
                            (North_ 0.5)
                            (Ext (South_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if isVertical m.north && isLowHorizontal m.east then
                    Sequence
                        [ Curve 1
                            (South_ 0.5)
                            (Ext (South_ 0.5) East)
                        , Line North (South_ 1.5)
                        ]

                else if isVertical m.north && isLowHorizontal m.west then
                    Sequence
                        [ Curve 1
                            (Ext South West)
                            (Ext (North_ 0.5) East)
                        , Line North (South_ 1.5)
                        ]

                else if isHorizontal m.east && isSlantLeft m.north_west then
                    Sequence
                        [ Curve 2
                            (Ext_ 0.5 North West)
                            (Ext (South_ 0.5) (East_ 1.5))
                        , Line
                            (Ext North West)
                            (Ext_ 0.5 South East)
                        ]

                else if isHorizontal m.east && isSlantRight m.north_east then
                    -- RoundCorner BottomLeftSlantedTopRight
                    Sequence
                        [ Curve 1
                            (Ext_ 0.5 North East)
                            (Ext_ 0.5 South East)
                        , Line
                            (Ext North East)
                            (Ext_ 0.5 South West)
                        ]

                else if isVertical m.north && isSlantLeft m.south_east then
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

                else if isHorizontal m.west && isSlantRight m.north_east then
                    Sequence
                        [ Curve 2
                            West
                            (Ext (North_ 0.5) (East_ 1.5))
                        , Line
                            (Ext_ 0.5 North East)
                            (Ext_ 0.5 North East)
                        ]

                else if isLowHorizontal m.east && isSlantRight m.north_east then
                    -- RoundCorner BottomLeftSlantedTopRightLowHorizontal
                    Sequence
                        [ Curve 1.5
                            (East_ 0)
                            (Ext South East)
                        , Line (Ext North East) (Ext South West)
                        ]

                else if isLowHorizontal m.west && isSlantLeft m.north_west then
                    -- RoundCorner BottomRightSlantedTopLeftLowHorizontal
                    Sequence
                        [ Curve 1.5
                            (Ext South West)
                            (Ext North East)
                        , Line
                            (Ext North West)
                            (Ext South East)
                        ]

                else if isHorizontal m.west && isSlantLeft m.north_west then
                    Sequence
                        [ Curve 1
                            West
                            (Ext_ 0.5 North East)
                        , Line
                            (Ext North West)
                            (Ext_ 0.5 South East)
                        ]

                else if isVertical m.north && isSlantRight m.south_west then
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

                else if isVertical m.north && isHorizontal m.west then
                    Sequence
                        [ Curve 1
                            West
                            (Ext (North_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if isHorizontal m.east && isRoundCorner m.south then
                    Sequence
                        [ Curve 1
                            East
                            (Ext (South_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if isHorizontal m.west && isRoundCorner m.south then
                    Sequence
                        [ Curve 1
                            (South_ 0.5)
                            (Ext (North_ 0.5) West)
                        , Line South (North_ 0.5)
                        ]

                else if isHorizontal m.west && isRoundCorner m.north then
                    Sequence
                        [ Curve 1
                            West
                            (Ext (North_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if isHorizontal m.east && isRoundCorner m.north then
                    Sequence
                        [ Curve 1
                            (North_ 0.5)
                            (Ext (South_ 0.5) East)
                        , Line North (South_ 0.5)
                        ]

                else if isHorizontal m.east && isSlantRight m.south_west then
                    Sequence
                        [ Curve 2
                            East
                            (Ext (South_ 0.5) (West_ 1.5))
                        , Line (Ext South West)
                            (Ext_ 0.5 North East)
                        ]

                else if isHorizontal m.east && isSlantLeft m.south_east then
                    Sequence
                        [ Curve 1
                            East
                            (Ext_ 0.5 South West)
                        , Line
                            (Ext South East)
                            (Ext_ 0.5 North West)
                        ]

                else if isHorizontal m.west && isSlantLeft m.south_east then
                    Sequence
                        [ Curve 2
                            (Ext_ 0.5 South East)
                            (Ext (North_ 0.5) (West_ 1.5))
                        , Line
                            (Ext South East)
                            (Ext_ 0.5 North West)
                        ]

                else if isHorizontal m.west && isSlantRight m.south_west then
                    --Todo
                    Sequence
                        [ Curve 1
                            (Ext_ 0.5 South West)
                            (Ext_ 0.5 North West)
                        , Line
                            (Ext South West)
                            (Ext_ 0.5 North East)
                        ]

                else if isVertical m.south && isSlantLeft m.north_west then
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

            else if isArrowRight center then
                Arrow West

            else if isArrowDown center then
                if isVertical m.north then
                    Arrow North

                else if isSlantRight m.north_east then
                    Arrow <| Ext North East

                else if isSlantLeft m.north_west then
                    Arrow <| Ext North West

                else
                    Text m.center

            else if isArrowLeft center then
                Arrow East

            else if isArrowUp center then
                if isVertical m.south then
                    Arrow South

                else if isSlantRight m.south_west then
                    Arrow <| Ext South West

                else if isSlantLeft m.south_east then
                    Arrow <| Ext South East

                else
                    Maybe.map Text center
                        |> Maybe.withDefault Empty

            else if isSlantRight center then
                Line
                    (Ext North East)
                    (Ext_ 2 South West)

            else if isSlantLeft center then
                Line
                    (Ext South East)
                    (Ext_ 2 North West)

            else if isOpenCurve center then
                if isSlantRight m.north_east && isSlantLeft m.south_east then
                    Curve 4
                        (Ext North East)
                        (Ext South South)

                else if
                    isRoundCorner m.north_east
                        && isRoundCorner m.south_east
                then
                    Curve 4
                        North
                        (Ext South South)

                else
                    Text m.center

            else if
                isCloseCurve center
                    && isRoundCorner m.north_west
                    && isRoundCorner m.south_west
            then
                Curve 4
                    South
                    (Ext North North)

            else if
                isCloseCurve center
                    && isSlantLeft m.north_west
                    && isSlantRight m.south_west
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
