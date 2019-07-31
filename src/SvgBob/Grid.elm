module SvgBob.Grid exposing (getSvg)

import Array exposing (Array)
import Char
import Color
import Dict exposing (Dict)
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
        , Scan(..)
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

        Center ->
            pt


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

        Center ->
            Center


type alias Matrix =
    { north_west : Scan
    , north : Scan
    , north_east : Scan
    , west : Scan
    , east : Scan
    , south_west : Scan
    , south : Scan
    , south_east : Scan
    }



--getMatrix : Int -> Int -> Dict Array (Array Char) -> Maybe Matrix


getMatrix x y dict =
    { north_west = get ( x - 1, y - 1 ) dict
    , north = get ( x, y - 1 ) dict
    , north_east = get ( x + 1, y - 1 ) dict
    , west = get ( x - 1, y ) dict
    , east = get ( x + 1, y ) dict
    , south_west = get ( x - 1, y + 1 ) dict
    , south = get ( x, y + 1 ) dict
    , south_east = get ( x + 1, y + 1 ) dict
    }


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


lowHorizontalLine : Char -> Matrix -> Element
lowHorizontalLine char { west, east, south_west, south_east } =
    if SlantRight == west then
        Line (Ext South East) (West_ 4)

    else if Vertical == west then
        Line (Ext South East) (West_ 3)

    else if SlantLeft == east then
        Line (Ext South West) (East_ 4)

    else if Vertical == east then
        Line (Ext South West) (East_ 3)

    else if Vertical == south_west then
        Line (Ext South (West_ 2)) (East_ 3)

    else if Vertical == south_east then
        Line (Ext South West) (East_ 3)

    else if (AlphaNumeric /= west) && (AlphaNumeric /= east) then
        Line (Ext South East) (West_ 2)

    else
        Text char


intersection : Char -> Matrix -> Element
intersection char { south, west, north, east, south_west, south_east } =
    let
        isVerticalJunctionLeft =
            (Vertical == north) && (Vertical == south) && (Horizontal == west)

        isVerticalJunctionRight =
            (Vertical == north) && (Vertical == south) && (Horizontal == east)

        isHorizontalJunctionTop =
            (Horizontal == west) && (Horizontal == east) && (Vertical == north)

        isHorizontalJunctionBot =
            (Horizontal == west) && (Horizontal == east) && (Vertical == south)

        isTopLeftIntersection =
            (Vertical == south) && (Horizontal == east)

        isTopRightIntersection =
            (Vertical == south) && (Horizontal == west)

        isBottomRightIntersection =
            (Vertical == north) && (Horizontal == west)

        isBottomLeftIntersection =
            (Vertical == north) && (Horizontal == east)

        isCrossIntersection =
            (Vertical == north)
                && (Vertical == south)
                && (Horizontal == west)
                && (Horizontal == east)
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


getElement m ( char, elem ) model =
    if
        elem
            == Vertical
            && (AlphaNumeric /= m.west)
            && (AlphaNumeric /= m.east)
    then
        Line South (Ext North North)

    else if
        Horizontal
            == elem
            && (AlphaNumeric /= m.west)
            && (AlphaNumeric /= m.east)
    then
        Line East (West_ 2)

    else if LowHorizontal == elem then
        lowHorizontalLine char m

    else if IntersectionX == elem then
        intersection char m

    else if RoundCorner == elem then
        if
            [ m.north_east, m.south_west, m.east ]
                == [ SlantRight, SlantRight, Horizontal ]
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
            (Vertical == m.north)
                && (Vertical == m.south)
                && (SlantLeft == m.north_west)
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
            (SlantLeft == m.north_west)
                && (SlantLeft == m.south_east)
                && (Horizontal == m.west)
        then
            -- RoundCorner SlantedLeftJunctionLeft
            Sequence
                [ Curve 2
                    (Ext_ 0.5 South East)
                    (Ext (North_ 0.5) (West_ 1.5))
                , Line (Ext North West) (Ext_ 2 South East)
                ]

        else if
            (SlantRight == m.north_east)
                && (SlantRight == m.south_west)
                && (Horizontal == m.west)
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
            (SlantLeft == m.north_west)
                && (SlantLeft == m.south_east)
                && (Horizontal == m.east)
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
            (Vertical == m.north)
                && (Vertical == m.south)
                && (SlantRight == m.south_west)
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
            (Vertical == m.north)
                && (Vertical == m.south)
                && (SlantLeft == m.south_east)
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
            (Vertical == m.north)
                && (Vertical == m.south)
                && (SlantRight == m.north_east)
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

        else if (Vertical == m.south) && (Horizontal == m.east) then
            Sequence
                [ Curve 1
                    East
                    (Ext (South_ 0.5) West)
                , Line South (North_ 0.5)
                ]

        else if (Vertical == m.south) && (Horizontal == m.west) then
            Sequence
                [ Curve 1
                    (South_ 0.5)
                    (Ext (North_ 0.5) West)
                , Line South (North_ 0.5)
                ]

        else if (Vertical == m.south) && (SlantRight == m.north_east) then
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

        else if (Horizontal == m.east) && (OpenCurve == m.south_west) then
            Curve 4
                East
                (Ext South (West_ 3))

        else if (RoundCorner == m.east) && (OpenCurve == m.south_west) then
            Curve 4
                East
                (Ext South (West_ 3))

        else if (Horizontal == m.west) && (CloseCurve == m.south_east) then
            Curve 4
                (Ext South (East_ 2))
                (Ext North (West_ 3))

        else if (RoundCorner == m.west) && (CloseCurve == m.south_east) then
            Curve 4
                (Ext South (East_ 2))
                (Ext North (West_ 3))

        else if (Horizontal == m.east) && (OpenCurve == m.north_west) then
            Curve 4
                (Ext North (West_ 2))
                (Ext South (East_ 3))

        else if (Horizontal == m.west) && (CloseCurve == m.north_east) then
            Curve 4
                West
                (Ext North (East_ 3))

        else if (RoundCorner == m.east) && (OpenCurve == m.north_west) then
            Curve 4
                (Ext North (West_ 2))
                (Ext South (East_ 3))

        else if (RoundCorner == m.west) && (CloseCurve == m.north_east) then
            Curve 4
                West
                (Ext North (East_ 3))

        else if (Vertical == m.north) && (Horizontal == m.east) then
            Sequence
                [ Curve 1
                    (North_ 0.5)
                    (Ext (South_ 0.5) East)
                , Line North (South_ 0.5)
                ]

        else if (Vertical == m.north) && (LowHorizontal == m.east) then
            Sequence
                [ Curve 1
                    (South_ 0.5)
                    (Ext (South_ 0.5) East)
                , Line North (South_ 1.5)
                ]

        else if (Vertical == m.north) && (LowHorizontal == m.west) then
            Sequence
                [ Curve 1
                    (Ext South West)
                    (Ext (North_ 0.5) East)
                , Line North (South_ 1.5)
                ]

        else if (Horizontal == m.east) && (SlantLeft == m.north_west) then
            Sequence
                [ Curve 2
                    (Ext_ 0.5 North West)
                    (Ext (South_ 0.5) (East_ 1.5))
                , Line
                    (Ext North West)
                    (Ext_ 0.5 South East)
                ]

        else if (Horizontal == m.east) && (SlantRight == m.north_east) then
            -- RoundCorner BottomLeftSlantedTopRight
            Sequence
                [ Curve 1
                    (Ext_ 0.5 North East)
                    (Ext_ 0.5 South East)
                , Line
                    (Ext North East)
                    (Ext_ 0.5 South West)
                ]

        else if (Vertical == m.north) && (SlantLeft == m.south_east) then
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

        else if (Horizontal == m.west) && (SlantRight == m.north_east) then
            Sequence
                [ Curve 2
                    West
                    (Ext (North_ 0.5) (East_ 1.5))
                , Line
                    (Ext_ 0.5 North East)
                    (Ext_ 0.5 North East)
                ]

        else if (LowHorizontal == m.east) && (SlantRight == m.north_east) then
            -- RoundCorner BottomLeftSlantedTopRightLowHorizontal
            Sequence
                [ Curve 1.5
                    Center
                    (Ext South East)
                , Line (Ext North East) (Ext South West)
                ]

        else if (LowHorizontal == m.west) && (SlantLeft == m.north_west) then
            -- RoundCorner BottomRightSlantedTopLeftLowHorizontal
            Sequence
                [ Curve 1.5
                    (Ext South West)
                    (Ext North East)
                , Line
                    (Ext North West)
                    (Ext South East)
                ]

        else if (Horizontal == m.west) && (SlantLeft == m.north_west) then
            Sequence
                [ Curve 1
                    West
                    (Ext_ 0.5 North East)
                , Line
                    (Ext North West)
                    (Ext_ 0.5 South East)
                ]

        else if (Vertical == m.north) && (SlantRight == m.south_west) then
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

        else if (Vertical == m.north) && (Horizontal == m.west) then
            Sequence
                [ Curve 1
                    West
                    (Ext (North_ 0.5) East)
                , Line North (South_ 0.5)
                ]

        else if (Horizontal == m.east) && (RoundCorner == m.south) then
            Sequence
                [ Curve 1
                    East
                    (Ext (South_ 0.5) West)
                , Line South (North_ 0.5)
                ]

        else if (Horizontal == m.west) && (RoundCorner == m.south) then
            Sequence
                [ Curve 1
                    (South_ 0.5)
                    (Ext (North_ 0.5) West)
                , Line South (North_ 0.5)
                ]

        else if (Horizontal == m.west) && (RoundCorner == m.north) then
            Sequence
                [ Curve 1
                    West
                    (Ext (North_ 0.5) East)
                , Line North (South_ 0.5)
                ]

        else if (Horizontal == m.east) && (RoundCorner == m.north) then
            Sequence
                [ Curve 1
                    (North_ 0.5)
                    (Ext (South_ 0.5) East)
                , Line North (South_ 0.5)
                ]

        else if (Horizontal == m.east) && (SlantRight == m.south_west) then
            Sequence
                [ Curve 2
                    East
                    (Ext (South_ 0.5) (West_ 1.5))
                , Line (Ext South West)
                    (Ext_ 0.5 North East)
                ]

        else if (Horizontal == m.east) && (SlantLeft == m.south_east) then
            Sequence
                [ Curve 1
                    East
                    (Ext_ 0.5 South West)
                , Line
                    (Ext South East)
                    (Ext_ 0.5 North West)
                ]

        else if (Horizontal == m.west) && (SlantLeft == m.south_east) then
            Sequence
                [ Curve 2
                    (Ext_ 0.5 South East)
                    (Ext (North_ 0.5) (West_ 1.5))
                , Line
                    (Ext South East)
                    (Ext_ 0.5 North West)
                ]

        else if (Horizontal == m.west) && (SlantRight == m.south_west) then
            --Todo
            Sequence
                [ Curve 1
                    (Ext_ 0.5 South West)
                    (Ext_ 0.5 North West)
                , Line
                    (Ext South West)
                    (Ext_ 0.5 North East)
                ]

        else if (Vertical == m.south) && (SlantLeft == m.north_west) then
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
            Text char

    else if ArrowRight == elem then
        Arrow West

    else if ArrowDown == elem then
        if Vertical == m.north then
            Arrow North

        else if SlantRight == m.north_east then
            Arrow <| Ext North East

        else if SlantLeft == m.north_west then
            Arrow <| Ext North West

        else
            Text char

    else if ArrowLeft == elem then
        Arrow East

    else if ArrowUp == elem then
        if Vertical == m.south then
            Arrow South

        else if SlantRight == m.south_west then
            Arrow <| Ext South West

        else if SlantLeft == m.south_east then
            Arrow <| Ext South East

        else
            Text char

    else if SlantRight == elem then
        Line
            (Ext North East)
            (Ext_ 2 South West)

    else if SlantLeft == elem then
        Line
            (Ext South East)
            (Ext_ 2 North West)

    else if OpenCurve == elem then
        if (SlantRight == m.north_east) && (SlantLeft == m.south_east) then
            Curve 4
                (Ext North East)
                (Ext South South)

        else if (RoundCorner == m.north_east) && (RoundCorner == m.south_east) then
            Curve 4
                North
                (Ext South South)

        else
            Text char

    else if
        (CloseCurve == elem)
            && (RoundCorner == m.north_west)
            && (RoundCorner == m.south_west)
    then
        Curve 4
            South
            (Ext North North)

    else if
        (CloseCurve == elem)
            && (SlantLeft == m.north_west)
            && (SlantRight == m.south_west)
    then
        Curve 4
            (Ext South West)
            (Ext North North)

    else
        Text char


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


drawElement : Dict ( Int, Int ) ( Char, Scan ) -> Model -> ( ( Int, Int ), ( Char, Scan ) ) -> List (Svg a)
drawElement dict model ( ( x, y ), ( char, element ) ) =
    let
        position =
            Point
                (measureX x + textWidth / 2)
                (measureY y + textHeight / 2)
    in
    case getElement (getMatrix x y dict) ( char, element ) model of
        Intersection itype ->
            drawIntersection x y itype model

        e ->
            draw model.settings position e


drawPaths : Model -> List (Svg a)
drawPaths model =
    let
        elements =
            model.lines
                |> List.indexedMap scanLine
                |> List.concat

        dict =
            Dict.fromList elements

        fn =
            drawElement dict model
    in
    List.map fn elements
        |> List.concat


scanLine y =
    List.indexedMap (scanElement y) >> List.filter filterElement


filterElement ( _, ( _, e ) ) =
    e /= None


scanElement : Int -> Int -> Char -> ( ( Int, Int ), ( Char, Scan ) )
scanElement y x char =
    ( ( x, y )
    , ( char
      , case char of
            '-' ->
                Horizontal

            '_' ->
                LowHorizontal

            '+' ->
                IntersectionX

            '.' ->
                RoundCorner

            '\'' ->
                RoundCorner

            ',' ->
                RoundCorner

            '`' ->
                RoundCorner

            '´' ->
                RoundCorner

            '>' ->
                ArrowRight

            '<' ->
                ArrowLeft

            'V' ->
                ArrowDown

            'v' ->
                ArrowDown

            '^' ->
                ArrowUp

            'î' ->
                ArrowUp

            '/' ->
                SlantRight

            '\\' ->
                SlantLeft

            '(' ->
                OpenCurve

            ')' ->
                CloseCurve

            '|' ->
                Vertical

            ' ' ->
                None

            _ ->
                AlphaNumeric
      )
    )


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

        _ ->
            dir


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


get pos dict =
    dict
        |> Dict.get pos
        |> Maybe.map Tuple.second
        |> Maybe.withDefault None
