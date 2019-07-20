module SvgBob.Grid exposing (getSvg)

import Array exposing (Array)
import Char
import Color
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute)
import String
import Svg exposing (Svg, defs, line, marker, path, svg)
import Svg.Attributes
    exposing
        ( d
        , fill
        , height
        , id
        , markerEnd
        , markerHeight
        , markerUnits
        , markerWidth
        , orient
        , refX
        , refY
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeWidth
        , viewBox
        , width
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )
import SvgBob.Model exposing (..)
import SvgBob.Types exposing (..)


type alias Point =
    { x : Float
    , y : Float
    }


mult : Int -> Direction -> Direction
mult i dir =
    if i <= 1 then
        dir

    else
        dir
            |> mult (i - 1)
            |> Ext dir


move : Direction -> Point -> Point
move dir pt =
    case dir of
        South ->
            { pt | y = pt.y + textHeight / 2 }

        North ->
            { pt | y = pt.y - textHeight / 2 }

        West ->
            { pt | x = pt.x + textWidth / 2 }

        East ->
            { pt | x = pt.x - textWidth / 2 }

        Ext dir1 dir2 ->
            pt
                |> move dir1
                |> move dir2


type alias Vector =
    { orientation : Point
    , position : Point
    }


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


leftOf x y model =
    get (x - 1) y model


rightOf x y model =
    get (x + 1) y model


topOf x y model =
    get x (y - 1) model


bottomOf x y model =
    get x (y + 1) model


topLeftOf x y model =
    get (x - 1) (y - 1) model


topRightOf x y model =
    get (x + 1) (y - 1) model


bottomLeftOf x y model =
    get (x - 1) (y + 1) model


bottomRightOf x y model =
    get (x + 1) (y + 1) model


isNeighbor neighbor check =
    case neighbor of
        Just neighbor_ ->
            check neighbor_

        Nothing ->
            False


getElement : Int -> Int -> Model -> Element
getElement x y model =
    let
        char =
            get x y model

        top =
            topOf x y model

        bottom =
            bottomOf x y model

        left =
            leftOf x y model

        right =
            rightOf x y model

        topLeft =
            topLeftOf x y model

        topRight =
            topRightOf x y model

        bottomLeft =
            bottomLeftOf x y model

        bottomRight =
            bottomRightOf x y model
    in
    case char of
        Nothing ->
            Empty

        Just char_ ->
            if
                isVertical char_
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Line South (Ext North North)

            else if
                isHorizontal char_
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Line West (Ext East East)

            else if
                isLowHorizontal char_
                    && isNeighbor left isSlantRight
            then
                Line (Ext South West) (mult 4 East)

            else if
                isLowHorizontal char_
                    && isNeighbor left isVertical
            then
                Line (Ext South West) (mult 3 East)

            else if
                isLowHorizontal char_
                    && isNeighbor right isSlantLeft
            then
                Line (Ext South East) (mult 4 West)

            else if
                isLowHorizontal char_
                    && isNeighbor right isVertical
            then
                Line (Ext South East) (mult 3 West)

            else if
                isLowHorizontal char_
                    && isNeighbor bottomLeft isVertical
            then
                Line (Ext South (Ext East East)) (mult 3 West)

            else if
                isLowHorizontal char_
                    && isNeighbor bottomRight isVertical
            then
                Line (Ext South East) (mult 3 West)

            else if
                isLowHorizontal char_
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Line (Ext South West) (Ext East East)

            else if isIntersection char_ then
                let
                    isVerticalJunctionLeft =
                        isNeighbor top isVertical
                            && isNeighbor (bottomOf x y model) isVertical
                            && isNeighbor (leftOf x y model) isHorizontal

                    isVerticalJunctionRight =
                        isNeighbor top isVertical
                            && isNeighbor bottom isVertical
                            && isNeighbor right isHorizontal

                    isHorizontalJunctionTop =
                        isNeighbor left isHorizontal
                            && isNeighbor right isHorizontal
                            && isNeighbor top isVertical

                    isHorizontalJunctionBot =
                        isNeighbor left isHorizontal
                            && isNeighbor right isHorizontal
                            && isNeighbor bottom isVertical

                    isTopLeftIntersection =
                        isNeighbor bottom isVertical && isNeighbor right isHorizontal

                    isTopRightIntersection =
                        isNeighbor bottom isVertical && isNeighbor left isHorizontal

                    isBottomRightIntersection =
                        isNeighbor top isVertical && isNeighbor left isHorizontal

                    isBottomLeftIntersection =
                        isNeighbor top isVertical && isNeighbor right isHorizontal

                    isCrossIntersection =
                        isNeighbor top isVertical
                            && isNeighbor bottom isVertical
                            && isNeighbor left isHorizontal
                            && isNeighbor right isHorizontal
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

            else if isRoundCorner char_ then
                if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor right isHorizontal
                then
                    RoundCorner SlantedRightJunctionRight

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft
                then
                    RoundCorner VerticalTopDownJunctionTopLeft

                else if
                    isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor left isHorizontal
                then
                    RoundCorner SlantedLeftJunctionLeft

                else if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor left isHorizontal
                then
                    RoundCorner SlantedRightJunctionLeft

                else if
                    isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor right isHorizontal
                then
                    RoundCorner SlantedLeftJunctionRight

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomLeft isSlantRight
                then
                    RoundCorner VerticalTopDownJunctionBottomLeft

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomRight isSlantLeft
                then
                    RoundCorner VerticalTopDownJunctionBottomRight

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight
                then
                    RoundCorner VerticalTopDownJunctionTopRight

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor right isHorizontal
                then
                    RoundCorner TopLeftCorner

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor left isHorizontal
                then
                    RoundCorner TopRightCorner

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight
                then
                    RoundCorner TopLeftSlantedTopRight

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isOpenCurve
                then
                    RoundCorner TopLeftBigCurve

                else if
                    isNeighbor right isRoundCorner
                        && isNeighbor bottomLeft isOpenCurve
                then
                    RoundCorner TopLeftBigCurve

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomRight isCloseCurve
                then
                    RoundCorner TopRightBigCurve

                else if
                    isNeighbor left isRoundCorner
                        && isNeighbor bottomRight isCloseCurve
                then
                    RoundCorner TopRightBigCurve

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topLeft isOpenCurve
                then
                    RoundCorner BottomLeftBigCurve

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topRight isCloseCurve
                then
                    RoundCorner BottomRightBigCurve

                else if
                    isNeighbor right isRoundCorner
                        && isNeighbor topLeft isOpenCurve
                then
                    RoundCorner BottomLeftBigCurve

                else if
                    isNeighbor left isRoundCorner
                        && isNeighbor topRight isCloseCurve
                then
                    RoundCorner BottomRightBigCurve

                else if
                    isNeighbor top isVertical
                        && isNeighbor right isHorizontal
                then
                    RoundCorner BottomLeftCorner

                else if
                    isNeighbor top isVertical
                        && isNeighbor right isLowHorizontal
                then
                    RoundCorner BottomLeftLowHorizontal

                else if
                    isNeighbor top isVertical
                        && isNeighbor left isLowHorizontal
                then
                    RoundCorner BottomRightLowHorizontal

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    RoundCorner BottomLeftSlantedTopLeft

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    RoundCorner BottomLeftSlantedTopRight

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottomRight isSlantLeft
                then
                    RoundCorner BottomLeftSlantedBottomRight

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    RoundCorner BottomRightSlantedTopRight

                else if
                    isNeighbor right isLowHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    RoundCorner BottomLeftSlantedTopRightLowHorizontal

                else if
                    isNeighbor left isLowHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    RoundCorner BottomRightSlantedTopLeftLowHorizontal

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    RoundCorner BottomRightSlantedTopLeft

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottomLeft isSlantRight
                then
                    RoundCorner BottomRightSlantedBottomLeft

                else if
                    isNeighbor top isVertical
                        && isNeighbor left isHorizontal
                then
                    RoundCorner BottomRightCorner

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottom isRoundCorner
                then
                    RoundCorner TopLeftCorner

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottom isRoundCorner
                then
                    RoundCorner TopRightCorner

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor top isRoundCorner
                then
                    RoundCorner BottomRightCorner

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor top isRoundCorner
                then
                    RoundCorner BottomLeftCorner

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isSlantRight
                then
                    RoundCorner TopLeftSlantedBottomLeft

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomRight isSlantLeft
                then
                    RoundCorner TopLeftSlantedBottomRight

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomRight isSlantLeft
                then
                    RoundCorner TopRightSlantedBottomRight

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomLeft isSlantRight
                then
                    RoundCorner TopRightSlantedBottomLeft

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft
                then
                    RoundCorner TopRightSlantedTopLeft

                else
                    Text char_

            else if isArrowRight char_ then
                Arrow East

            else if isArrowDown char_ then
                if isNeighbor top isVertical then
                    Arrow North

                else if isNeighbor topRight isSlantRight then
                    Arrow <| Ext North West

                else if isNeighbor topLeft isSlantLeft then
                    Arrow <| Ext North East

                else
                    Text char_

            else if isArrowLeft char_ then
                Arrow West

            else if isArrowUp char_ then
                if isNeighbor bottom isVertical then
                    Arrow South

                else if isNeighbor bottomLeft isSlantRight then
                    Arrow <| Ext South East

                else if isNeighbor bottomRight isSlantLeft then
                    Arrow <| Ext South West

                else
                    Text char_

            else if isSlantRight char_ then
                Line (Ext North West) (mult 2 (Ext South East))

            else if isSlantLeft char_ then
                Line (Ext South West) (mult 2 (Ext North East))

            else if isOpenCurve char_ then
                if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomRight isSlantLeft
                then
                    Curve (Ext North West) (Ext South South)

                else if
                    isNeighbor topRight isRoundCorner
                        && isNeighbor bottomRight isRoundCorner
                then
                    Curve North (Ext South South)

                else
                    Text char_

            else if
                isCloseCurve char_
                    && isNeighbor topLeft isRoundCorner
                    && isNeighbor bottomLeft isRoundCorner
            then
                Curve South (Ext North North)

            else if
                isCloseCurve char_
                    && isNeighbor topLeft isSlantLeft
                    && isNeighbor bottomLeft isSlantRight
            then
                Curve (Ext South East) (Ext North North)

            else if char_ /= ' ' then
                Text char_

            else
                Empty


vectorEffect : Attribute a
vectorEffect =
    attribute "vector-effect" "none"


drawArc : Float -> Float -> Float -> Float -> Float -> Settings -> Svg a
drawArc startX startY endX endY radius s =
    let
        rx =
            radius

        ry =
            radius

        paths =
            [ "M"
            , String.fromFloat startX
            , String.fromFloat startY
            , "A"
            , String.fromFloat rx
            , String.fromFloat ry
            , "0"
            , "0"
            , "0"
            , String.fromFloat endX
            , String.fromFloat endY
            ]
                |> String.join " "
    in
    path [ d paths, stroke "black", strokeWidth <| String.fromFloat s.lineWidth, fill "transparent", vectorEffect ] []


drawArcX : Settings -> Float -> Point -> Direction -> Svg a
drawArcX s faktor pos dir =
    let
        pos2 =
            move dir pos

        radius =
            s.arcRadius * faktor
    in
    path
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
            |> d
        , stroke "black"
        , strokeWidth <| String.fromFloat s.lineWidth
        , fill "transparent"
        , vectorEffect
        ]
        []


arrowMarker : Svg a
arrowMarker =
    marker
        [ id "triangle"
        , viewBox "0 0 14 14"
        , refX "0"
        , refY "5"
        , markerUnits "strokeWidth"
        , markerWidth "10"
        , markerHeight "10"
        , orient "auto"
        ]
        [ path [ d "M 0 0 L 10 5 L 0 10 z", vectorEffect ]
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
    svg (viewBox ("0 0 " ++ gwidth ++ " " ++ gheight) :: attr)
        (defs []
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
                    (\c char ->
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
        Line start stop ->
            [ drawLineX
                model.settings
                (move start position)
                stop
            ]

        Intersection itype ->
            drawIntersection x y itype model

        RoundCorner pos ->
            drawRoundCorner x y pos model.settings

        Arrow dir ->
            [ drawArrowX model.settings position dir ]

        Curve start stop ->
            [ drawArcX
                model.settings
                4
                (move start position)
                stop
            ]

        Text char ->
            [ drawText x y char model.settings ]

        Empty ->
            []


opposite : Direction -> Direction
opposite dir =
    case dir of
        West ->
            East

        East ->
            West

        North ->
            South

        South ->
            North

        Ext dir1 dir2 ->
            Ext (opposite dir1) (opposite dir2)


drawBigCloseCurve : Int -> Int -> Settings -> List (Svg a)
drawBigCloseCurve x y settings =
    let
        startX =
            measureX x + textWidth / 2

        startY =
            measureY y + textHeight

        endX =
            measureX x + textWidth / 2

        endY =
            measureY y
    in
    [ drawArc startX startY endX endY (settings.arcRadius * 4) settings ]


drawRoundCorner : Int -> Int -> Position -> Settings -> List (Svg a)
drawRoundCorner x y pos settings =
    case pos of
        TopLeftCorner ->
            drawRoundTopLeftCorner x y settings

        TopRightCorner ->
            drawRoundTopRightCorner x y settings

        BottomLeftCorner ->
            drawRoundBottomLeftCorner x y settings

        BottomRightCorner ->
            drawRoundBottomRightCorner x y settings

        TopLeftSlantedBottomLeft ->
            drawRoundTopLeftSlantedBottomLeftCorner x y settings

        TopLeftSlantedBottomRight ->
            drawRoundTopLeftSlantedBottomRightCorner x y settings

        TopRightSlantedBottomRight ->
            drawRoundTopRightSlantedBottomRight x y settings

        TopRightSlantedBottomLeft ->
            drawRoundTopRightSlantedBottomLeft x y settings

        TopRightSlantedTopLeft ->
            drawRoundTopRightSlantedTopLeft x y settings

        VerticalTopDownJunctionTopLeft ->
            drawVerticalTopDownJunctionTopLeft x y settings

        SlantedRightJunctionRight ->
            drawRoundSlantedRightJunctionRight x y settings

        SlantedLeftJunctionLeft ->
            drawRoundSlantedLeftJunctionLeft x y settings

        SlantedRightJunctionLeft ->
            drawRoundSlantedRightJunctionLeft x y settings

        SlantedLeftJunctionRight ->
            drawRoundSlantedLeftJunctionRight x y settings

        BottomLeftLowHorizontal ->
            drawRoundBottomLeftLowHorizontalCorner x y settings

        BottomRightLowHorizontal ->
            drawRoundBottomRightLowHorizontalCorner x y settings

        BottomLeftSlantedTopLeft ->
            drawRoundBottomLeftSlantedTopLeftCorner x y settings

        BottomLeftSlantedTopRight ->
            drawRoundBottomLeftSlantedTopRightCorner x y settings

        BottomLeftSlantedBottomRight ->
            drawRoundBottomLeftSlantedBottomRightCorner x y settings

        BottomLeftSlantedTopRightLowHorizontal ->
            drawRoundBottomLeftSlantedTopRightLowHorizontal x y settings

        BottomRightSlantedTopRight ->
            drawRoundBottomRightSlantedTopRightCorner x y settings

        BottomRightSlantedTopLeftLowHorizontal ->
            drawRoundBottomRightSlantedTopLeftLowHorizontal x y settings

        BottomRightSlantedTopLeft ->
            drawRoundBottomRightSlantedTopLeftCorner x y settings

        BottomRightSlantedBottomLeft ->
            drawRoundBottomRightSlantedBottomLeft x y settings

        VerticalTopDownJunctionBottomLeft ->
            drawVerticalTopDownJunctionBottomLeft x y settings

        VerticalTopDownJunctionBottomRight ->
            drawVerticalTopDownJunctionBottomRight x y settings

        TopLeftSlantedTopRight ->
            drawRoundTopLeftSlantedTopRightCorner x y settings

        VerticalTopDownJunctionTopRight ->
            drawVerticalTopDownJunctionTopRight x y settings

        TopLeftBigCurve ->
            drawTopLeftBigCurve x y settings

        TopRightBigCurve ->
            drawTopRightBigCurve x y settings

        BottomLeftBigCurve ->
            drawBottomLeftBigCurve x y settings

        BottomRightBigCurve ->
            drawBottomRightBigCurve x y settings


drawRoundTopLeftCorner x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight / 2

        endX =
            measureX x + textWidth / 2

        --circular arc
        endY =
            measureY y + textHeight / 2 + textWidth / 2

        --then the rest is line
    in
    [ drawArc startX startY endX endY s.arcRadius s
    , drawLine endX endY endX (measureY y + textHeight) s
    ]


drawTopLeftBigCurve x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight / 2

        endX =
            measureX x - textWidth / 2

        endY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY (s.arcRadius * 4) s ]


drawBottomLeftBigCurve x y s =
    let
        startX =
            measureX x - textWidth / 2

        startY =
            measureY y

        endX =
            measureX x + textWidth

        endY =
            measureY y + textHeight / 2
    in
    [ drawArc startX startY endX endY (s.arcRadius * 4) s ]


drawBottomRightBigCurve x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight / 2

        endX =
            measureX x + textWidth + textWidth / 2

        endY =
            measureY y
    in
    [ drawArc startX startY endX endY (s.arcRadius * 4) s ]


drawTopRightBigCurve x y s =
    let
        startX =
            measureX x + textWidth + textWidth / 2

        startY =
            measureY y + textHeight

        endX =
            measureX x

        endY =
            measureY y + textHeight / 2
    in
    [ drawArc startX startY endX endY (s.arcRadius * 4) s ]


drawRoundTopLeftSlantedTopRightCorner x y s =
    let
        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth * 3 / 4

        lendY =
            measureY y + textHeight * 1 / 4

        l2startX =
            measureX x + textWidth / 2

        l2startY =
            measureY y + textHeight

        l2endX =
            measureX x + textWidth / 2

        l2endY =
            measureY y + textHeight * 3 / 4
    in
    [ drawArc lendX lendY l2endX l2endY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    ]


drawVerticalTopDownJunctionTopRight x y s =
    let
        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth * 3 / 4

        lendY =
            measureY y + textHeight * 1 / 4

        l2startX =
            measureX x + textWidth / 2

        l2startY =
            measureY y + textHeight

        l2endX =
            measureX x + textWidth / 2

        l2endY =
            measureY y + textHeight * 3 / 4

        l3startX =
            measureX x + textWidth / 2

        l3startY =
            measureY y

        l3endX =
            measureX x + textWidth / 2

        l3endY =
            measureY y + textHeight
    in
    [ drawArc lendX lendY l2endX l2endY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    , drawLine l3startX l3startY l3endX l3endY s
    ]


drawRoundTopRightSlantedBottomRight x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y + textHeight

        lendX =
            measureX x + textWidth * 3 / 4

        lendY =
            measureY y + textHeight * 3 / 4
    in
    [ drawArc lendX lendY startX startY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundTopRightSlantedBottomLeft x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x

        lstartY =
            measureY y + textHeight

        lendX =
            measureX x + textWidth * 1 / 4

        lendY =
            measureY y + textHeight * 3 / 4
    in
    [ drawArc lendX lendY startX startY (s.arcRadius * 3 / 4) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundTopRightSlantedTopLeft x y s =
    let
        lstartX =
            measureX x + textWidth / 2

        lstartY =
            measureY y + textHeight

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight * 3 / 4

        l2startX =
            measureX x

        l2startY =
            measureY y

        l2endX =
            measureX x + textWidth * 1 / 4

        l2endY =
            measureY y + textHeight * 1 / 4
    in
    [ drawArc lendX lendY l2endX l2endY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    ]


drawVerticalTopDownJunctionTopLeft x y s =
    let
        lstartX =
            measureX x + textWidth / 2

        lstartY =
            measureY y + textHeight

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight * 3 / 4

        l2startX =
            measureX x

        l2startY =
            measureY y

        l2endX =
            measureX x + textWidth * 1 / 4

        l2endY =
            measureY y + textHeight * 1 / 4

        l3startX =
            measureX x + textWidth / 2

        l3startY =
            measureY y

        l3endX =
            measureX x + textWidth / 2

        l3endY =
            measureY y + textHeight
    in
    [ drawArc lendX lendY l2endX l2endY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    , drawLine l3startX l3startY l3endX l3endY s
    ]


drawRoundTopLeftSlantedBottomLeftCorner x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x

        lstartY =
            measureY y + textHeight

        lendX =
            measureX x + textWidth * 1 / 4

        lendY =
            measureY y + textHeight * 3 / 4
    in
    [ drawArc startX startY lendX lendY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundTopLeftSlantedBottomRightCorner x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y + textHeight

        lendX =
            measureX x + textWidth * 3 / 4

        lendY =
            measureY y + textHeight * 3 / 4
    in
    [ drawArc startX startY lendX lendY (s.arcRadius * 3 / 4) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundBottomLeftSlantedTopLeftCorner x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth * 1 / 4

        lendY =
            measureY y + textHeight * 1 / 4
    in
    [ drawArc lendX lendY startX startY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundBottomLeftSlantedTopRightCorner x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth * 3 / 4

        lendY =
            measureY y + textHeight * 1 / 4
    in
    [ drawArc lendX lendY startX startY (s.arcRadius * 3 / 4) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundBottomLeftSlantedBottomRightCorner x y s =
    let
        lstartX =
            measureX x + textWidth / 2

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight * 1 / 4

        l2startX =
            measureX x + textWidth

        l2startY =
            measureY y + textHeight

        l2endX =
            measureX x + textWidth * 3 / 4

        l2endY =
            measureY y + textHeight * 3 / 4
    in
    [ drawArc lendX lendY l2endX l2endY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    ]


drawRoundBottomLeftSlantedTopRightLowHorizontal x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight

        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight / 2
    in
    [ drawArc lendX lendY startX startY (s.arcRadius * 1.5) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundBottomRightSlantedTopLeftLowHorizontal x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight

        lstartX =
            measureX x

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight / 2
    in
    [ drawArc startX startY lendX lendY (s.arcRadius * 1.5) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundSlantedRightJunctionRight x y s =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight / 2

        endX =
            measureX x + textWidth * 1 / 4

        endY =
            measureY y + textHeight * 3 / 4

        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y

        lendX =
            measureX x

        lendY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundSlantedRightJunctionLeft x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight / 2

        endX =
            measureX x + textWidth * 3 / 4

        endY =
            measureY y + textHeight * 1 / 4

        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y

        lendX =
            measureX x

        lendY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundSlantedLeftJunctionLeft x y s =
    let
        startX =
            measureX x + textWidth * 3 / 4

        startY =
            measureY y + textHeight * 3 / 4

        endX =
            measureX x

        endY =
            measureY y + textHeight / 2

        lstartX =
            measureX x

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth

        lendY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundSlantedLeftJunctionRight x y s =
    let
        startX =
            measureX x + textWidth * 1 / 4

        startY =
            measureY y + textHeight * 1 / 4

        endX =
            measureX x + textWidth

        endY =
            measureY y + textHeight / 2

        lstartX =
            measureX x

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth

        lendY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundBottomRightSlantedTopRightCorner x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x + textWidth

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth * 3 / 4

        lendY =
            measureY y + textHeight * 1 / 4
    in
    [ drawArc startX startY lendX lendY (s.arcRadius * 2) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundBottomRightSlantedTopLeftCorner x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight / 2

        lstartX =
            measureX x

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth * 1 / 4

        lendY =
            measureY y + textHeight * 1 / 4
    in
    [ drawArc startX startY lendX lendY (s.arcRadius * 3 / 4) s
    , drawLine lstartX lstartY lendX lendY s
    ]


drawRoundBottomRightSlantedBottomLeft x y s =
    let
        lstartX =
            measureX x + textWidth / 2

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight * 1 / 4

        l2startX =
            measureX x

        l2startY =
            measureY y + textHeight

        l2endX =
            measureX x + textWidth * 1 / 4

        l2endY =
            measureY y + textHeight * 3 / 4
    in
    [ drawArc l2endX l2endY lendX lendY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    ]


drawVerticalTopDownJunctionBottomLeft x y s =
    let
        lstartX =
            measureX x + textWidth / 2

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight * 1 / 4

        l2startX =
            measureX x

        l2startY =
            measureY y + textHeight

        l2endX =
            measureX x + textWidth * 1 / 4

        l2endY =
            measureY y + textHeight * 3 / 4

        l3startX =
            measureX x + textWidth / 2

        l3startY =
            measureY y

        l3endX =
            measureX x + textWidth / 2

        l3endY =
            measureY y + textHeight
    in
    [ drawArc l2endX l2endY lendX lendY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    , drawLine l3startX l3startY l3endX l3endY s
    ]


drawVerticalTopDownJunctionBottomRight x y s =
    let
        lstartX =
            measureX x + textWidth / 2

        lstartY =
            measureY y

        lendX =
            measureX x + textWidth / 2

        lendY =
            measureY y + textHeight * 1 / 4

        l2startX =
            measureX x + textWidth

        l2startY =
            measureY y + textHeight

        l2endX =
            measureX x + textWidth * 3 / 4

        l2endY =
            measureY y + textHeight * 3 / 4

        l3startX =
            measureX x + textWidth / 2

        l3startY =
            measureY y

        l3endX =
            measureX x + textWidth / 2

        l3endY =
            measureY y + textHeight
    in
    [ drawArc lendX lendY l2endX l2endY (s.arcRadius * 4) s
    , drawLine lstartX lstartY lendX lendY s
    , drawLine l2startX l2startY l2endX l2endY s
    , drawLine l3startX l3startY l3endX l3endY s
    ]


drawRoundBottomLeftCorner x y s =
    let
        startX =
            measureX x + textWidth / 2

        startY =
            measureY y + textHeight / 2 - textWidth / 2

        endX =
            measureX x + textWidth

        endY =
            measureY y + textHeight / 2
    in
    [ drawArc startX startY endX endY s.arcRadius s
    , drawLine startX startY startX (measureY y) s
    ]


drawRoundBottomLeftLowHorizontalCorner x y s =
    let
        startX =
            measureX x + textWidth / 2

        startY =
            measureY y + textHeight - textWidth / 2

        endX =
            measureX x + textWidth

        endY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY s.arcRadius s
    , drawLine startX startY startX (measureY y) s
    ]


drawRoundBottomRightLowHorizontalCorner x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight

        endX =
            measureX x + textWidth / 2

        endY =
            measureY y + textHeight - textWidth / 2
    in
    [ drawArc startX startY endX endY s.arcRadius s
    , drawLine endX endY endX (measureY y) s
    ]


drawRoundTopRightCorner x y s =
    let
        startX =
            measureX x + textWidth / 2

        startY =
            measureY y + textWidth / 2 + textHeight / 2

        endX =
            measureX x

        endY =
            measureY y + textHeight / 2
    in
    [ drawArc startX startY endX endY s.arcRadius s
    , drawLine startX startY startX (measureY y + textHeight) s
    ]


drawRoundBottomRightCorner : Int -> Int -> Settings -> List (Svg a)
drawRoundBottomRightCorner x y s =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight / 2

        endX =
            measureX x + textWidth / 2

        endY =
            measureY y + textHeight / 2 - textWidth / 2
    in
    [ drawArc startX startY endX endY s.arcRadius s
    , drawLine endX endY endX (measureY y) s
    ]


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


drawArrowX : Settings -> Point -> Direction -> Svg a
drawArrowX settings pos dir =
    toLine
        [ Svg.Attributes.style
            ("stroke: "
                ++ colorText settings.color
                ++ ";stroke-width:"
                ++ String.fromFloat settings.lineWidth
            )
        , markerEnd "url(#triangle)"
        , vectorEffect
        ]
        (move dir pos)
        (opposite dir)


drawLine : Float -> Float -> Float -> Float -> Settings -> Svg a
drawLine startX startY endX endY s =
    line
        [ x1 <| String.fromFloat startX
        , x2 <| String.fromFloat endX
        , y1 <| String.fromFloat startY
        , y2 <| String.fromFloat endY
        , stroke <| colorText s.color
        , strokeWidth <| String.fromFloat s.lineWidth
        , strokeLinecap "round"
        , strokeLinejoin "mitter"
        , vectorEffect
        ]
        []


toLine : List (Svg.Attribute msg) -> Point -> Direction -> Svg msg
toLine misc pos dir =
    let
        pos2 =
            move dir pos
    in
    line
        (List.append
            misc
            [ x1 <| String.fromFloat pos.x
            , x2 <| String.fromFloat pos2.x
            , y1 <| String.fromFloat pos.y
            , y2 <| String.fromFloat pos2.y
            ]
        )
        []


drawLineX : Settings -> Point -> Direction -> Svg a
drawLineX s =
    toLine
        [ stroke <| colorText s.color
        , strokeWidth <| String.fromFloat s.lineWidth
        , strokeLinecap "round"
        , strokeLinejoin "mitter"
        , vectorEffect
        ]


drawText : Int -> Int -> Char -> Settings -> Svg a
drawText x_ y_ char s =
    let
        x__ =
            measureX x_ - textWidth / 4

        y__ =
            measureY y_ + textHeight * 3 / 4
    in
    Svg.node "text"
        [ x <| String.fromFloat x__
        , y <| String.fromFloat y__
        , Svg.Attributes.style ("font-size:" ++ String.fromFloat s.fontSize ++ "px;font-family:monospace")
        ]
        [ Svg.text <| String.fromChar char ]


measureX : Int -> Float
measureX x =
    toFloat x * textWidth


measureY : Int -> Float
measureY y =
    toFloat y * textHeight


get : Int -> Int -> { a | lines : Array (Array c) } -> Maybe c
get x y { lines } =
    lines
        |> Array.get y
        |> Maybe.map (Array.get x)
        |> Maybe.withDefault Nothing
