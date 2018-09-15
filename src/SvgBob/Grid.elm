module SvgBob.Grid exposing (getSvg)

import Array exposing (Array)
import Char
import Color
import Html exposing (Html)
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


textWidth =
    8.0


textHeight =
    16.0


vertical =
    [ '|' ]


verticalDashed =
    [ ':' ]


horizontal =
    [ '-' ]


horizontalDouble =
    [ '=' ]


lowHorizontal =
    [ '_' ]


intersections =
    [ '+' ]


roundCorners =
    [ '.', '\'' ]


arrowRight =
    [ '>' ]


arrowDown =
    [ 'V', 'v' ]


arrowLeft =
    [ '<' ]


arrowUp =
    [ '^', 'î' ]


slantRight =
    [ '/' ]


slantLeft =
    [ '\\' ]


openCurve =
    [ '(' ]


closeCurve =
    [ ')' ]


isOpenCurve char =
    List.member char openCurve



--close parenthesis


isCloseCurve char =
    List.member char closeCurve


isVertical char =
    List.member char vertical


isAlphaNumeric char =
    Char.isDigit char || Char.isUpper char || Char.isLower char


isHorizontal char =
    List.member char horizontal


isLowHorizontal char =
    List.member char lowHorizontal


isIntersection char =
    List.member char intersections


isLine char =
    isVertical char || isHorizontal char || isLowHorizontal char


isRoundCorner char =
    List.member char roundCorners


isArrowRight char =
    List.member char arrowRight


isArrowLeft char =
    List.member char arrowLeft


isArrowDown char =
    List.member char arrowDown


isArrowUp char =
    List.member char arrowUp


isSlantRight char =
    List.member char slantRight


isSlantLeft char =
    List.member char slantLeft


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
        Just neighbor ->
            check neighbor

        Nothing ->
            False


getElement : Int -> Int -> Model -> Maybe Element
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
        Just char ->
            if
                isVertical char
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Just Vertical

            else if
                isHorizontal char
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Just Horizontal

            else if
                isLowHorizontal char
                    && isNeighbor left isSlantRight
            then
                Just LowHorizontalExtendLeft

            else if
                isLowHorizontal char
                    && isNeighbor left isVertical
            then
                Just LowHorizontalExtendVerticalLeft

            else if
                isLowHorizontal char
                    && isNeighbor right isSlantLeft
            then
                Just LowHorizontalExtendRight

            else if
                isLowHorizontal char
                    && isNeighbor right isVertical
            then
                Just LowHorizontalExtendVerticalRight

            else if
                isLowHorizontal char
                    && isNeighbor bottomLeft isVertical
            then
                Just LowHorizontalExtendVerticalBottomLeft

            else if
                isLowHorizontal char
                    && isNeighbor bottomRight isVertical
            then
                Just LowHorizontalExtendVerticalBottomRight

            else if
                isLowHorizontal char
                    && not (isNeighbor left isAlphaNumeric)
                    && not (isNeighbor right isAlphaNumeric)
            then
                Just LowHorizontal

            else if isIntersection char then
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
                    Just (Intersection Cross)

                else if isVerticalJunctionLeft then
                    Just (Intersection VertJunctionLeft)

                else if isVerticalJunctionRight then
                    Just (Intersection VertJunctionRight)

                else if isHorizontalJunctionTop then
                    Just (Intersection HorJunctionTop)

                else if isHorizontalJunctionBot then
                    Just (Intersection HorJunctionBot)

                else if isTopRightIntersection then
                    Just (Intersection TopRight)

                else if isTopLeftIntersection then
                    Just (Intersection TopLeft)

                else if isBottomRightIntersection then
                    Just (Intersection BottomRight)

                else if isBottomLeftIntersection then
                    Just (Intersection BottomLeft)

                else
                    Nothing

            else if isRoundCorner char then
                if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor right isHorizontal
                then
                    Just (RoundCorner SlantedRightJunctionRight)

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft
                then
                    Just (RoundCorner VerticalTopDownJunctionTopLeft)

                else if
                    isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor left isHorizontal
                then
                    Just (RoundCorner SlantedLeftJunctionLeft)

                else if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomLeft isSlantRight
                        && isNeighbor left isHorizontal
                then
                    Just (RoundCorner SlantedRightJunctionLeft)

                else if
                    isNeighbor topLeft isSlantLeft
                        && isNeighbor bottomRight isSlantLeft
                        && isNeighbor right isHorizontal
                then
                    Just (RoundCorner SlantedLeftJunctionRight)

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomLeft isSlantRight
                then
                    Just (RoundCorner VerticalTopDownJunctionBottomLeft)

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor bottomRight isSlantLeft
                then
                    Just (RoundCorner VerticalTopDownJunctionBottomRight)

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight
                then
                    Just (RoundCorner VerticalTopDownJunctionTopRight)

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor right isHorizontal
                then
                    Just (RoundCorner TopLeftCorner)

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor left isHorizontal
                then
                    Just (RoundCorner TopRightCorner)

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor topRight isSlantRight
                then
                    Just (RoundCorner TopLeftSlantedTopRight)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isOpenCurve
                then
                    Just (RoundCorner TopLeftBigCurve)

                else if
                    isNeighbor right isRoundCorner
                        && isNeighbor bottomLeft isOpenCurve
                then
                    Just (RoundCorner TopLeftBigCurve)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomRight isCloseCurve
                then
                    Just (RoundCorner TopRightBigCurve)

                else if
                    isNeighbor left isRoundCorner
                        && isNeighbor bottomRight isCloseCurve
                then
                    Just (RoundCorner TopRightBigCurve)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topLeft isOpenCurve
                then
                    Just (RoundCorner BottomLeftBigCurve)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topRight isCloseCurve
                then
                    Just (RoundCorner BottomRightBigCurve)

                else if
                    isNeighbor right isRoundCorner
                        && isNeighbor topLeft isOpenCurve
                then
                    Just (RoundCorner BottomLeftBigCurve)

                else if
                    isNeighbor left isRoundCorner
                        && isNeighbor topRight isCloseCurve
                then
                    Just (RoundCorner BottomRightBigCurve)

                else if
                    isNeighbor top isVertical
                        && isNeighbor right isHorizontal
                then
                    Just (RoundCorner BottomLeftCorner)

                else if
                    isNeighbor top isVertical
                        && isNeighbor right isLowHorizontal
                then
                    Just (RoundCorner BottomLeftLowHorizontal)

                else if
                    isNeighbor top isVertical
                        && isNeighbor left isLowHorizontal
                then
                    Just (RoundCorner BottomRightLowHorizontal)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    Just (RoundCorner BottomLeftSlantedTopLeft)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    Just (RoundCorner BottomLeftSlantedTopRight)

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottomRight isSlantLeft
                then
                    Just (RoundCorner BottomLeftSlantedBottomRight)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    Just (RoundCorner BottomRightSlantedTopRight)

                else if
                    isNeighbor right isLowHorizontal
                        && isNeighbor topRight isSlantRight
                then
                    Just (RoundCorner BottomLeftSlantedTopRightLowHorizontal)

                else if
                    isNeighbor left isLowHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    Just (RoundCorner BottomRightSlantedTopLeftLowHorizontal)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor topLeft isSlantLeft
                then
                    Just (RoundCorner BottomRightSlantedTopLeft)

                else if
                    isNeighbor top isVertical
                        && isNeighbor bottomLeft isSlantRight
                then
                    Just (RoundCorner BottomRightSlantedBottomLeft)

                else if
                    isNeighbor top isVertical
                        && isNeighbor left isHorizontal
                then
                    Just (RoundCorner BottomRightCorner)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottom isRoundCorner
                then
                    Just (RoundCorner TopLeftCorner)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottom isRoundCorner
                then
                    Just (RoundCorner TopRightCorner)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor top isRoundCorner
                then
                    Just (RoundCorner BottomRightCorner)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor top isRoundCorner
                then
                    Just (RoundCorner BottomLeftCorner)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomLeft isSlantRight
                then
                    Just (RoundCorner TopLeftSlantedBottomLeft)

                else if
                    isNeighbor right isHorizontal
                        && isNeighbor bottomRight isSlantLeft
                then
                    Just (RoundCorner TopLeftSlantedBottomRight)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomRight isSlantLeft
                then
                    Just (RoundCorner TopRightSlantedBottomRight)

                else if
                    isNeighbor left isHorizontal
                        && isNeighbor bottomLeft isSlantRight
                then
                    Just (RoundCorner TopRightSlantedBottomLeft)

                else if
                    isNeighbor bottom isVertical
                        && isNeighbor topLeft isSlantLeft
                then
                    Just (RoundCorner TopRightSlantedTopLeft)

                else
                    Just (Text char)

            else if isArrowRight char then
                Just ArrowEast

            else if isArrowDown char then
                if isNeighbor top isVertical then
                    Just ArrowSouth

                else if isNeighbor topRight isSlantRight then
                    Just ArrowSouthWest

                else if isNeighbor topLeft isSlantLeft then
                    Just ArrowSouthEast

                else
                    Just <| Text char

            else if isArrowLeft char then
                Just ArrowWest

            else if isArrowUp char then
                if isNeighbor bottom isVertical then
                    Just ArrowNorth

                else if isNeighbor bottomLeft isSlantRight then
                    Just ArrowNorthWest

                else if isNeighbor bottomRight isSlantLeft then
                    Just ArrowNorthEast

                else
                    Just <| Text char

            else if isSlantRight char then
                Just SlantRight

            else if isSlantLeft char then
                Just SlantLeft

            else if isOpenCurve char then
                if
                    isNeighbor topRight isSlantRight
                        && isNeighbor bottomRight isSlantLeft
                then
                    Just OpenCurve

                else if
                    isNeighbor topRight isRoundCorner
                        && isNeighbor bottomRight isRoundCorner
                then
                    Just BigOpenCurve

                else
                    Just <| Text char

            else if
                isCloseCurve char
                    && isNeighbor topLeft isRoundCorner
                    && isNeighbor bottomLeft isRoundCorner
            then
                Just BigCloseCurve

            else if
                isCloseCurve char
                    && isNeighbor topLeft isSlantLeft
                    && isNeighbor bottomLeft isSlantRight
            then
                Just CloseCurve

            else if char /= ' ' then
                Just <| Text char

            else
                Nothing

        Nothing ->
            Nothing


drawArc : Float -> Float -> Float -> Float -> Float -> Settings -> Svg a
drawArc startX startY endX endY radius s =
    let
        rx =
            radius

        ry =
            radius

        paths =
            [ "M"
            , toString startX
            , toString startY
            , "A"
            , toString rx
            , toString ry
            , "0"
            , "0"
            , "0"
            , toString endX
            , toString endY
            ]
                |> String.join " "
    in
    path [ d paths, stroke "black", strokeWidth <| toString s.lineWidth, fill "transparent" ] []


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
        [ path [ d "M 0 0 L 10 5 L 0 10 z" ]
            []
        ]


getSvg : Model -> Html msg
getSvg model =
    let
        gwidth =
            toString <| measureX model.columns + 10

        gheight =
            toString <| measureY model.rows + 10
    in
    svg [ height gheight, width gwidth ]
        (defs []
            [ arrowMarker ]
            :: drawPaths model
        )



--TODO: optimize here to indexedMap only the lines and chars
--TODO: modularized parts in order to easily fit and match


drawPaths : Model -> List (Svg a)
drawPaths model =
    Array.indexedMap
        (\r line ->
            Array.indexedMap
                (\c char ->
                    drawElement c r model
                )
                line
                |> Array.toList
        )
        model.lines
        |> Array.toList
        |> List.concat
        |> List.concat


drawElement : Int -> Int -> Model -> List (Svg a)
drawElement x y model =
    let
        element =
            getElement x y model
    in
    case element of
        Just element ->
            case element of
                Horizontal ->
                    [ drawHorizontalLine x y model ]

                LowHorizontal ->
                    [ drawLowHorizontalLine x y model ]

                LowHorizontalExtendLeft ->
                    [ drawLowHorizontalExtendLeft x y model ]

                LowHorizontalExtendVerticalLeft ->
                    [ drawLowHorizontalExtendVerticalLeft x y model ]

                LowHorizontalExtendRight ->
                    [ drawLowHorizontalExtendRight x y model ]

                LowHorizontalExtendVerticalRight ->
                    [ drawLowHorizontalExtendVerticalRight x y model ]

                LowHorizontalExtendVerticalBottomLeft ->
                    [ drawLowHorizontalExtendVerticalBottomLeft x y model ]

                LowHorizontalExtendVerticalBottomRight ->
                    [ drawLowHorizontalExtendVerticalBottomRight x y model ]

                Vertical ->
                    [ drawVerticalLine x y model ]

                Intersection itype ->
                    drawIntersection x y itype model

                RoundCorner pos ->
                    drawRoundCorner x y pos model

                ArrowEast ->
                    [ drawArrowRight x y model ]

                ArrowSouth ->
                    [ drawArrowDown x y model ]

                ArrowSouthWest ->
                    [ drawArrowSouthWest x y model ]

                ArrowSouthEast ->
                    [ drawArrowSouthEast x y model ]

                ArrowNorth ->
                    [ drawArrowUp x y model ]

                ArrowNorthWest ->
                    [ drawArrowNorthWest x y model ]

                ArrowNorthEast ->
                    [ drawArrowNorthEast x y model ]

                ArrowWest ->
                    [ drawArrowLeft x y model ]

                SlantRight ->
                    [ drawSlantRightLine x y model ]

                SlantLeft ->
                    [ drawSlantLeftLine x y model ]

                OpenCurve ->
                    drawOpenCurve x y model

                CloseCurve ->
                    drawCloseCurve x y model

                BigOpenCurve ->
                    drawBigOpenCurve x y model

                BigCloseCurve ->
                    drawBigCloseCurve x y model

                Text char ->
                    [ drawText x y char model.settings ]

        Nothing ->
            []


drawHorizontalLine : Int -> Int -> Model -> Svg a
drawHorizontalLine x y model =
    let
        startX =
            measureX x

        endX =
            startX + textWidth

        startY =
            measureY y + textHeight / 2

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawLowHorizontalLine : Int -> Int -> Model -> Svg a
drawLowHorizontalLine x y model =
    let
        startX =
            measureX x

        endX =
            startX + textWidth

        startY =
            measureY y + textHeight

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawLowHorizontalExtendLeft : Int -> Int -> Model -> Svg a
drawLowHorizontalExtendLeft x y model =
    let
        startX =
            measureX x - textWidth

        endX =
            measureX x + textWidth

        startY =
            measureY y + textHeight

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawLowHorizontalExtendVerticalLeft : Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalLeft x y model =
    let
        startX =
            measureX x - textWidth / 2

        endX =
            measureX x + textWidth

        startY =
            measureY y + textHeight

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawLowHorizontalExtendVerticalBottomLeft : Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalBottomLeft x y model =
    let
        startX =
            measureX x - textWidth / 2

        endX =
            measureX x + textWidth

        startY =
            measureY y + textHeight

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawLowHorizontalExtendVerticalBottomRight : Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalBottomRight x y model =
    let
        startX =
            measureX x

        endX =
            measureX x + textWidth + textWidth / 2

        startY =
            measureY y + textHeight

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawLowHorizontalExtendRight : Int -> Int -> Model -> Svg a
drawLowHorizontalExtendRight x y model =
    let
        startX =
            measureX x

        endX =
            measureX x + textWidth * 2

        startY =
            measureY y + textHeight

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawLowHorizontalExtendVerticalRight : Int -> Int -> Model -> Svg a
drawLowHorizontalExtendVerticalRight x y model =
    let
        startX =
            measureX x

        endX =
            measureX x + textWidth + textWidth / 2

        startY =
            measureY y + textHeight

        endY =
            startY
    in
    drawLine startX startY endX endY model.settings


drawVerticalLine : Int -> Int -> Model -> Svg a
drawVerticalLine x y model =
    let
        startX =
            measureX x + textWidth / 2

        endX =
            startX

        startY =
            measureY y

        endY =
            startY + textHeight
    in
    drawLine startX startY endX endY model.settings


drawSlantRightLine : Int -> Int -> Model -> Svg a
drawSlantRightLine x y model =
    let
        startX =
            measureX x

        endX =
            startX + textWidth

        startY =
            measureY y + textHeight

        endY =
            measureY y
    in
    drawLine startX startY endX endY model.settings


drawSlantLeftLine : Int -> Int -> Model -> Svg a
drawSlantLeftLine x y model =
    let
        startX =
            measureX x

        endX =
            startX + textWidth

        startY =
            measureY y

        endY =
            measureY y + textHeight
    in
    drawLine startX startY endX endY model.settings


drawOpenCurve : Int -> Int -> Model -> List (Svg a)
drawOpenCurve x y model =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y

        endX =
            measureX x + textWidth

        endY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY (model.settings.arcRadius * 4) model.settings ]


drawBigOpenCurve : Int -> Int -> Model -> List (Svg a)
drawBigOpenCurve x y model =
    let
        startX =
            measureX x + textWidth / 2

        startY =
            measureY y

        endX =
            measureX x + textWidth / 2

        endY =
            measureY y + textHeight
    in
    [ drawArc startX startY endX endY (model.settings.arcRadius * 4) model.settings ]


drawBigCloseCurve : Int -> Int -> Model -> List (Svg a)
drawBigCloseCurve x y model =
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
    [ drawArc startX startY endX endY (model.settings.arcRadius * 4) model.settings ]


drawCloseCurve : Int -> Int -> Model -> List (Svg a)
drawCloseCurve x y model =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight

        endX =
            measureX x

        endY =
            measureY y

        radius =
            textHeight
    in
    [ drawArc startX startY endX endY radius model.settings ]


drawRoundCorner : Int -> Int -> Position -> Model -> List (Svg a)
drawRoundCorner x y pos model =
    case pos of
        TopLeftCorner ->
            drawRoundTopLeftCorner x y model.settings

        TopRightCorner ->
            drawRoundTopRightCorner x y model.settings

        BottomLeftCorner ->
            drawRoundBottomLeftCorner x y model.settings

        BottomRightCorner ->
            drawRoundBottomRightCorner x y model.settings

        TopLeftSlantedBottomLeft ->
            drawRoundTopLeftSlantedBottomLeftCorner x y model.settings

        TopLeftSlantedBottomRight ->
            drawRoundTopLeftSlantedBottomRightCorner x y model.settings

        TopRightSlantedBottomRight ->
            drawRoundTopRightSlantedBottomRight x y model.settings

        TopRightSlantedBottomLeft ->
            drawRoundTopRightSlantedBottomLeft x y model.settings

        TopRightSlantedTopLeft ->
            drawRoundTopRightSlantedTopLeft x y model.settings

        VerticalTopDownJunctionTopLeft ->
            drawVerticalTopDownJunctionTopLeft x y model.settings

        SlantedRightJunctionRight ->
            drawRoundSlantedRightJunctionRight x y model.settings

        SlantedLeftJunctionLeft ->
            drawRoundSlantedLeftJunctionLeft x y model.settings

        SlantedRightJunctionLeft ->
            drawRoundSlantedRightJunctionLeft x y model.settings

        SlantedLeftJunctionRight ->
            drawRoundSlantedLeftJunctionRight x y model.settings

        BottomLeftLowHorizontal ->
            drawRoundBottomLeftLowHorizontalCorner x y model.settings

        BottomRightLowHorizontal ->
            drawRoundBottomRightLowHorizontalCorner x y model.settings

        BottomLeftSlantedTopLeft ->
            drawRoundBottomLeftSlantedTopLeftCorner x y model.settings

        BottomLeftSlantedTopRight ->
            drawRoundBottomLeftSlantedTopRightCorner x y model.settings

        BottomLeftSlantedBottomRight ->
            drawRoundBottomLeftSlantedBottomRightCorner x y model.settings

        BottomLeftSlantedTopRightLowHorizontal ->
            drawRoundBottomLeftSlantedTopRightLowHorizontal x y model.settings

        BottomRightSlantedTopRight ->
            drawRoundBottomRightSlantedTopRightCorner x y model.settings

        BottomRightSlantedTopLeftLowHorizontal ->
            drawRoundBottomRightSlantedTopLeftLowHorizontal x y model.settings

        BottomRightSlantedTopLeft ->
            drawRoundBottomRightSlantedTopLeftCorner x y model.settings

        BottomRightSlantedBottomLeft ->
            drawRoundBottomRightSlantedBottomLeft x y model.settings

        VerticalTopDownJunctionBottomLeft ->
            drawVerticalTopDownJunctionBottomLeft x y model.settings

        VerticalTopDownJunctionBottomRight ->
            drawVerticalTopDownJunctionBottomRight x y model.settings

        TopLeftSlantedTopRight ->
            drawRoundTopLeftSlantedTopRightCorner x y model.settings

        VerticalTopDownJunctionTopRight ->
            drawVerticalTopDownJunctionTopRight x y model.settings

        TopLeftBigCurve ->
            drawTopLeftBigCurve x y model.settings

        TopRightBigCurve ->
            drawTopRightBigCurve x y model.settings

        BottomLeftBigCurve ->
            drawBottomLeftBigCurve x y model.settings

        BottomRightBigCurve ->
            drawBottomRightBigCurve x y model.settings


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


drawArrowRight x y model =
    let
        startX =
            measureX x

        endX =
            startX + textWidth / 2

        startY =
            measureY y + textHeight / 2

        endY =
            startY

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawArrowLeft x y model =
    let
        startX =
            measureX x + textWidth

        endX =
            measureX x + textWidth / 2

        startY =
            measureY y + textHeight / 2

        endY =
            startY

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawArrowDown x y model =
    let
        startX =
            measureX x + textWidth / 2

        endX =
            startX

        startY =
            measureY y

        endY =
            startY + textHeight

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawArrowSouthWest x y model =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y

        endX =
            startX - textWidth / 2

        endY =
            startY + textHeight / 2

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawArrowSouthEast x y model =
    let
        startX =
            measureX x

        startY =
            measureY y

        endX =
            startX + textWidth / 2

        endY =
            startY + textHeight / 2

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawArrowUp x y model =
    let
        startX =
            measureX x + textWidth / 2

        endX =
            startX

        startY =
            measureY y + textHeight

        endY =
            measureY y

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawArrowNorthWest x y model =
    let
        startX =
            measureX x

        startY =
            measureY y + textHeight

        endX =
            startX + textWidth / 2

        endY =
            measureY y + textHeight / 2

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawArrowNorthEast x y model =
    let
        startX =
            measureX x + textWidth

        startY =
            measureY y + textHeight

        endX =
            measureX x + textWidth / 2

        endY =
            measureY y + textHeight / 2

        { red, green, blue, alpha } =
            Color.toRgb model.settings.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , Svg.Attributes.style ("stroke: " ++ colorText ++ ";stroke-width:" ++ toString model.settings.lineWidth)
        , markerEnd "url(#triangle)"
        ]
        []


drawLine startX startY endX endY s =
    let
        { red, green, blue, alpha } =
            Color.toRgb s.color

        colorText =
            "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"
    in
    line
        [ x1 <| toString startX
        , x2 <| toString endX
        , y1 <| toString startY
        , y2 <| toString endY
        , stroke colorText
        , strokeWidth <| toString s.lineWidth
        , strokeLinecap "round"
        , strokeLinejoin "mitter"
        ]
        []


drawText : Int -> Int -> Char -> Settings -> Svg a
drawText x_ y_ char s =
    let
        x__ =
            measureX x_ - textWidth / 4

        y__ =
            measureY y_ + textHeight * 3 / 4
    in
    Svg.node "text"
        [ x <| toString x__
        , y <| toString y__
        , Svg.Attributes.style ("font-size:" ++ toString s.fontSize ++ "px;font-family:monospace")
        ]
        [ Svg.text <| String.fromChar char ]


measureX : Int -> Float
measureX x =
    toFloat x * textWidth


measureY : Int -> Float
measureY y =
    toFloat y * textHeight


get : Int -> Int -> Model -> Maybe Char
get x y model =
    let
        row =
            y

        line : Maybe (Array Char)
        line =
            Array.get y model.lines

        char : Maybe Char
        char =
            case line of
                Just l ->
                    Array.get x l

                Nothing ->
                    Nothing
    in
    char
