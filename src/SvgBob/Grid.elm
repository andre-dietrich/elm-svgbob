module SvgBob.Grid exposing (drawElements, getElements, getSvg)

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute)
import String
import Svg exposing (Svg)
import Svg.Attributes as Attr
import SvgBob.Model exposing (Model, Settings, init)
import SvgBob.Scanner exposing (getScans)
import SvgBob.Types
    exposing
        ( Direction(..)
        , Element(..)
        , Point
        , Scan(..)
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

        Center ->
            pt

        Ext dir1 dir2 ->
            let
                next =
                    pt
                        |> move dir1
                        |> move dir2
            in
            next

        Ext_ n dir1 dir2 ->
            let
                next =
                    pt
                        |> move (moveExt n dir1)
                        |> move (moveExt n dir2)
            in
            next


moveExt : Float -> Direction -> Direction
moveExt n dir =
    case dir of
        Center ->
            Center

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
            let
                next =
                    Ext (moveExt n dir1) (moveExt n dir2)
            in
            next

        Ext_ m dir1 dir2 ->
            let
                next =
                    Ext (moveExt (n * m) dir1) (moveExt (n * m) dir2)
            in
            next


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


getMatrix : Int -> Int -> Dict ( Int, Int ) ( String, Scan ) -> Matrix
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


apply : Matrix -> List ( Matrix -> Bool, Element ) -> List Element
apply matrix list =
    apply_ matrix list []


apply_ : Matrix -> List ( Matrix -> Bool, Element ) -> List Element -> List Element
apply_ matrix input output =
    case input of
        [] ->
            List.reverse output

        ( if_, then_ ) :: fns ->
            if if_ matrix then
                apply_ matrix fns (then_ :: output)

            else
                apply_ matrix fns output


lowHorizontal : String -> Matrix -> Element
lowHorizontal char matrix =
    [ ( .west >> (==) SlantRight
      , Line (Ext South East) (West_ 4)
      )
    , ( .west >> (==) Vertical
      , Line (Ext South East) (West_ 3)
      )
    , ( .east >> (==) SlantLeft
      , Line (Ext South West) (East_ 4)
      )
    , ( .east >> (==) SlantLeft
      , Line (Ext South West) (East_ 4)
      )
    , ( .east >> (==) Vertical
      , Line (Ext South West) (East_ 3)
      )
    , ( .south_west >> (==) Vertical
      , Line (Ext South (West_ 2)) (East_ 3)
      )
    , ( .south_east >> (==) Vertical
      , Line (Ext South West) (East_ 3)
      )
    , ( \m -> (AlphaNumeric /= m.west) && (AlphaNumeric /= m.east)
      , Line (Ext South East) (West_ 2)
      )
    , ( \m -> OpenCurve == m.south_west
      , Curve 1 (Ext South West) (Ext_ 0.15 South (West_ 4))
      )
    , ( \m -> OpenCurve == m.west
      , Curve 1 (Ext (South_ 0.75) (West_ 1.6)) (Ext_ 0.2 South (East_ 4))
      )
    , ( \m -> CloseCurve == m.south_east
      , Curve 1 (Ext (South_ 1.15) (East_ 1.7)) (Ext_ 0.15 North (West_ 4))
      )
    , ( \m -> CloseCurve == m.east
      , Curve 1 (Ext South (East_ 1)) (Ext_ 0.2 (North_ 2) (East_ 4))
      )
    ]
        |> apply matrix
        |> sequenceWithDefault char


sequenceWithDefault : String -> List Element -> Element
sequenceWithDefault char list =
    if list == [] then
        Text char

    else
        Sequence list


intersection : String -> Matrix -> Element
intersection char matrix =
    [ ( \{ north } -> north == Vertical || north == Intersection || north == Corner
      , Line Center North
      )
    , ( \m -> m.south == Vertical || m.south == Intersection || m.south == Corner
      , Line Center South
      )
    , ( .east >> (==) Horizontal, Line Center East )
    , ( .west >> (==) Horizontal, Line Center West )
    , ( .north_west >> (==) SlantLeft, Line Center (Ext North West) )
    , ( .north_east >> (==) SlantRight, Line Center (Ext North East) )
    , ( .south_west >> (==) SlantRight, Line Center (Ext South West) )
    , ( .south_east >> (==) SlantLeft, Line Center (Ext South East) )
    , ( .west >> (==) Square, BigBox )
    , ( .east >> (==) Square, BigBox )
    , ( .south >> (==) Square, BigBox )
    , ( .north >> (==) Square, BigBox )
    ]
        |> apply matrix
        |> sequenceWithDefault char


closeCurve : String -> Matrix -> Element
closeCurve char matrix =
    [ ( \m -> Corner == m.north_west && Corner == m.south_west
      , Curve 4 South (Ext North North)
      )
    , ( \m -> SlantLeft == m.north_west && SlantRight == m.south_west
      , Curve 4 (Ext South West) (Ext North North)
      )
    ]
        |> apply matrix
        |> sequenceWithDefault char


openCurve : String -> Matrix -> Element
openCurve char matrix =
    [ ( \m -> Corner == m.north_east && Corner == m.south_east
      , Curve 4 North (Ext South South)
      )
    , ( \m -> SlantRight == m.north_east && SlantLeft == m.south_east
      , Curve 4 (Ext North East) (Ext South South)
      )
    ]
        |> apply matrix
        |> sequenceWithDefault char


corner : String -> Matrix -> Element
corner char matrix =
    [ ( \m -> (Horizontal == m.west) && (Horizontal == m.east)
      , Line West (East_ 2)
      )
    , ( \m -> (Vertical == m.north) && (Vertical == m.south)
      , Line North (South_ 2)
      )
    , ( \m -> Corner == m.north_east && Corner == m.south_west
      , Line Center (Ext North (East_ 2))
      )
    , ( \m -> (SlantRight == m.north_east) && (SlantRight == m.south_west)
      , Line (Ext North East) (Ext_ 2 South West)
      )
    , ( \m -> (SlantLeft == m.north_west) && (SlantLeft == m.south_east)
      , Line (Ext North West) (Ext_ 2 South East)
      )
    , ( \m -> (Vertical == m.north || Corner == m.north || Intersection == m.north) && (Horizontal == m.west)
      , Sequence
            [ Curve 1 West (Ext (North_ 0.5) East)
            , Line North (South_ 0.5)
            ]
      )
    , ( \m -> (Vertical == m.north || Corner == m.north || Intersection == m.north) && (Horizontal == m.east)
      , Sequence
            [ Curve 1 (North_ 0.5) (Ext (South_ 0.5) East)
            , Line North (South_ 0.5)
            ]
      )
    , ( \m -> (Vertical == m.south || Corner == m.south || Intersection == m.south) && (Horizontal == m.west)
      , Sequence
            [ Curve 1 (South_ 0.5) (Ext (North_ 0.5) West)
            , Line South (North_ 0.5)
            ]
      )
    , ( \m -> (Vertical == m.north || Corner == m.north || Intersection == m.north) && (LowHorizontal == m.west)
      , Sequence
            [ Curve 1 (Ext South West) (Ext (North_ 0.5) East)
            , Line North (South_ 1.5)
            ]
      )
    , ( \m -> (Vertical == m.south || Corner == m.south || Intersection == m.south) && (LowHorizontal == m.west)
      , Curve 1 (South_ 1.5) (Ext (North_ 0.5) West)
      )
    , ( \m -> (Vertical == m.south || Corner == m.south || Intersection == m.south) && (LowHorizontal == m.east)
      , Curve 1 (Ext South East) (Ext (South_ 0.5) West)
      )
    , ( \m -> (Vertical == m.north || Corner == m.north || Intersection == m.north) && (LowHorizontal == m.east)
      , Sequence
            [ Curve 1 (South_ 0.5) (Ext (South_ 0.5) East)
            , Line North (South_ 1.5)
            ]
      )
    , ( \m -> (Vertical == m.south || Corner == m.south || Intersection == m.south) && (Horizontal == m.east)
      , Sequence
            [ Curve 1 East (Ext (South_ 0.5) West)
            , Line South (North_ 0.5)
            ]
      )
    , ( \m -> (SlantLeft == m.south_east) && (Horizontal == m.west)
      , Curve 3 (Ext South East) (Ext_ 2 (North_ 0.5) West)
      )
    , ( \m -> (SlantRight == m.north_east) && (Horizontal == m.west)
      , Curve 3 West (Ext North (East_ 2))
      )
    , ( \m -> (SlantLeft == m.north_west) && (Horizontal == m.east)
      , Curve 3 (Ext North West) (Ext South (East_ 2))
      )
    , ( \m -> (SlantRight == m.north_west) && (Horizontal == m.east)
      , Curve 3 (Ext North West) (Ext South (East_ 20))
      )
    , ( \m -> (SlantRight == m.south_west) && (Horizontal == m.east)
      , Curve 3 East (Ext South (West_ 2))
      )
    , ( \m -> (Vertical == m.north) && (SlantRight == m.south_west)
      , Curve 8 (Ext South West) (Ext_ 2 North (East_ 0.5))
      )
    , ( \m -> (Vertical == m.north) && (SlantLeft == m.south_east)
      , Curve 8 North (Ext_ 2 South (East_ 0.5))
      )
    , ( \m -> (Vertical == m.south) && (SlantRight == m.north_east)
      , Curve 8 (Ext North East) (Ext_ 2 South (West_ 0.5))
      )
    , ( \m -> (Vertical == m.south) && (SlantLeft == m.north_west)
      , Curve 8 South (Ext_ 2 North (West_ 0.5))
      )
    , ( \m -> (Horizontal == m.east) && (SlantRight == m.north_east)
      , Curve 2 (Ext North East) South
      )
    , ( \m -> (Horizontal == m.west) && (SlantLeft == m.north_west)
      , Curve 2 West North
      )
    , ( \m -> (Horizontal == m.west) && (SlantRight == m.south_west)
      , Curve 2 (Ext South West) North
      )
    , ( \m -> (Horizontal == m.east) && (SlantLeft == m.south_east)
      , Curve 2 East South
      )
    , ( \m -> (LowHorizontal == m.east) && (SlantRight == m.north_east)
      , Curve 4 (Ext North East) (South_ 2)
      )
    , ( \m -> (LowHorizontal == m.west) && (SlantLeft == m.north_west)
      , Curve 4 (Ext South West) (North_ 2)
      )
    , ( \m -> (Horizontal == m.west) && (CloseCurve == m.south_east)
      , Curve 4 (Ext South (East_ 2)) (Ext North (West_ 3))
      )
    , ( \m -> OpenCurve == m.south_west
      , Curve 4 East (Ext South (West_ 3))
      )
    , ( \m -> CloseCurve == m.south_east
      , Curve 4 (Ext South (East_ 2)) (Ext North (West_ 3))
      )
    , ( \m -> OpenCurve == m.north_west
      , Curve 4 (Ext North (West_ 2)) (Ext South (East_ 3))
      )
    , ( \m -> CloseCurve == m.north_east
      , Curve 4 West (Ext North (East_ 3))
      )
    , ( \m -> Vertical == m.north && SlantRight == m.north_east
      , Sequence [ Line Center North, Line Center (Ext North East) ]
      )
    , ( \m -> Vertical == m.north && SlantLeft == m.north_west
      , Sequence [ Line Center North, Line Center (Ext North West) ]
      )
    , ( \m -> SlantRight == m.north_east && SlantLeft == m.north_west
      , Sequence [ Line Center (Ext North East), Line Center (Ext North West) ]
      )
    , ( \m -> m.west == LowHorizontal && m.east == Horizontal
      , Line (Ext South West) (Ext North (East_ 2))
      )
    , ( \m -> m.west == Horizontal && m.east == LowHorizontal
      , Line (Ext South East) (Ext North (West_ 2))
      )
    , ( \m -> m.west == Horizontal && m.north_east == LowHorizontal
      , Line West (Ext North (East_ 2))
      )
    , ( \m -> m.east == Horizontal && m.north_west == LowHorizontal
      , Line East (Ext North (West_ 2))
      )
    ]
        |> apply matrix
        |> sequenceWithDefault char


horizontal : String -> Matrix -> Element
horizontal char matrix =
    [ ( \m -> AlphaNumeric /= m.west || AlphaNumeric /= m.east
      , Line East (West_ 2)
      )
    ]
        |> apply matrix
        |> sequenceWithDefault char


getElement : Matrix -> ( String, Scan ) -> Element
getElement m ( char, elem ) =
    case elem of
        Vertical ->
            if AlphaNumeric /= m.west || AlphaNumeric /= m.east then
                Line South (Ext North North)

            else
                Text char

        Horizontal ->
            horizontal char m

        LowHorizontal ->
            lowHorizontal char m

        Intersection ->
            intersection char m

        Arrow South ->
            if Vertical == m.north then
                Triangle North

            else if SlantRight == m.north_east then
                Triangle <| Ext North East

            else if SlantLeft == m.north_west then
                Triangle <| Ext North West

            else
                Text char

        Arrow North ->
            if Vertical == m.south then
                Triangle South

            else if SlantRight == m.south_west then
                Triangle <| Ext South West

            else if SlantLeft == m.south_east then
                Triangle <| Ext South East

            else
                Text char

        Arrow East ->
            if Horizontal == m.west || Horizontal == m.east then
                Triangle East

            else
                Text char

        Arrow West ->
            if Horizontal == m.west || Horizontal == m.east then
                Triangle West

            else
                Text char

        Corner ->
            corner char m

        SlantRight ->
            Line (Ext North East) (Ext_ 2 South West)

        SlantLeft ->
            Line (Ext South East) (Ext_ 2 North West)

        OpenCurve ->
            openCurve char m

        CloseCurve ->
            closeCurve char m

        Square ->
            case intersection char m of
                Sequence list ->
                    Box
                        :: list
                        |> Sequence

                _ ->
                    Text char

        O filled ->
            circle filled char m

        Verbatim str ->
            str
                |> String.lines
                |> SvgBob.Model.dim
                |> ForeignObject str

        Emoji ->
            TextEmoji char

        _ ->
            Text char


circle : Bool -> String -> Matrix -> Element
circle filled char m =
    if AlphaNumeric == m.west || AlphaNumeric == m.east then
        Text char

    else
        case intersection char m of
            Sequence list ->
                List.append list [ Circle filled ]
                    |> Sequence

            _ ->
                Text char


vectorEffect : Attribute a
vectorEffect =
    attribute "vector-effect" "none"


drawArc : Settings -> Float -> Point -> Direction -> Svg msg
drawArc s factor pos dir =
    let
        pos2 =
            move dir pos

        radius =
            s.arcRadius * factor
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
        , Attr.stroke s.color.stroke
        , Attr.strokeWidth <| String.fromFloat s.lineWidth
        , Attr.fill "transparent"
        , vectorEffect
        ]
        []


arrowMarker : String -> Svg msg
arrowMarker c =
    Svg.marker
        [ Attr.id "triangle"
        , Attr.viewBox "0 0 14 14"
        , Attr.refX "0"
        , Attr.refY "5"
        , Attr.markerUnits "strokeWidth"
        , Attr.markerWidth "10"
        , Attr.markerHeight "10"
        , Attr.orient "auto"
        , Attr.stroke c
        , Attr.fill c
        ]
        [ Svg.path [ Attr.d "M 0 0 L 10 5 L 0 10 z", vectorEffect ] []
        ]


getSvg : Settings -> List (Svg.Attribute msg) -> Maybe (String -> Svg msg) -> String -> Html msg
getSvg settings attributes verbatim code =
    let
        model =
            init settings code
    in
    Svg.svg
        (viewBox model.rows model.columns
            :: bgColor model.settings.color.background
            :: attributes
        )
        (Svg.defs []
            [ arrowMarker model.settings.color.stroke ]
            :: drawPaths verbatim model
        )


drawElements :
    List (Svg.Attribute msg)
    -> (a -> Svg msg)
    ->
        { svg : List ( Point, Element )
        , foreign : List ( a, ( Point, ( Int, Int ) ) )
        , settings : Settings
        , columns : Int
        , rows : Int
        }
    -> Html msg
drawElements attributes verbatim config =
    let
        fnSVG =
            draw Nothing config.settings

        fnCustom =
            drawCustomObject verbatim config.settings
    in
    Svg.svg
        (viewBox config.rows config.columns
            :: bgColor config.settings.color.background
            :: attributes
        )
        (Svg.defs []
            [ arrowMarker config.settings.color.stroke ]
            :: (List.concatMap fnSVG config.svg
                    |> List.append (List.map (\( a, ( point, dim ) ) -> fnCustom point dim a) config.foreign)
               )
        )


viewBox : Int -> Int -> Attribute msg
viewBox rows columns =
    ("0 0 "
        ++ (String.fromFloat <| measureX columns + 10)
        ++ " "
        ++ (String.fromFloat <| measureY rows + 10)
    )
        |> Attr.viewBox


bgColor : String -> Svg.Attribute msg
bgColor bg =
    Attr.style ("background-color:" ++ bg ++ ";")


drawElement : Maybe (String -> Svg msg) -> Dict ( Int, Int ) ( String, Scan ) -> Settings -> ( ( Int, Int ), ( String, Scan ) ) -> List (Svg msg)
drawElement withVerbatim dict settings ( ( x, y ), ( char, element ) ) =
    draw withVerbatim
        settings
        ( Point
            (measureX x + textWidth / 2)
            (measureY y + textHeight / 2)
        , getElement (getMatrix x y dict) ( char, element )
        )


getElements :
    Settings
    -> String
    ->
        { rows : Int
        , columns : Int
        , settings : Settings
        , svg : List ( Point, Element )
        , foreign : List ( String, ( Point, ( Int, Int ) ) )
        }
getElements settings code =
    let
        model =
            init settings code

        intermediate =
            getScans
                model.settings.verbatim
                model.lines

        dict =
            Dict.fromList intermediate
    in
    intermediate
        |> List.foldl
            (\( ( x, y ), ( char, element ) ) container ->
                let
                    point =
                        Point
                            (measureX x + textWidth / 2)
                            (measureY y + textHeight / 2)
                in
                case getElement (getMatrix x y dict) ( char, element ) of
                    ForeignObject str dim ->
                        { container | foreign = ( str, ( point, dim ) ) :: container.foreign }

                    e ->
                        { container | svg = ( point, e ) :: container.svg }
            )
            { rows = model.rows
            , columns = model.columns
            , svg = []
            , foreign = []
            , settings = settings
            }


drawPaths : Maybe (String -> Svg msg) -> Model -> List (Svg msg)
drawPaths verbatim model =
    let
        settings =
            model.settings.verbatim

        intermediate =
            getScans
                { settings
                    | string =
                        case verbatim of
                            Nothing ->
                                ""

                            Just _ ->
                                settings.string
                }
                model.lines

        dict =
            Dict.fromList intermediate

        fn =
            drawElement verbatim dict model.settings
    in
    List.concatMap fn intermediate


draw : Maybe (String -> Svg msg) -> Settings -> ( Point, Element ) -> List (Svg msg)
draw withVerbatim settings ( pos, element ) =
    case element of
        Triangle dir ->
            [ drawArrow settings pos dir ]

        Text char ->
            [ drawText settings pos char ]

        TextEmoji char ->
            [ drawText settings (move (East_ 0.25) pos) char ]

        Line start stop ->
            [ drawLine settings (move start pos) stop ]

        Curve factor start stop ->
            [ drawArc settings factor (move start pos) stop ]

        Sequence elements ->
            let
                fn =
                    Tuple.pair pos >> draw withVerbatim settings
            in
            List.concatMap fn elements

        Box ->
            [ drawSquare False settings pos ]

        BigBox ->
            [ drawSquare True settings pos ]

        Circle filled ->
            [ Svg.circle
                [ Attr.cx <| String.fromFloat pos.x
                , Attr.cy <| String.fromFloat pos.y
                , Attr.r <| String.fromFloat settings.arcRadius
                , Attr.fill <|
                    if filled then
                        settings.color.stroke

                    else
                        settings.color.background
                , Attr.stroke settings.color.stroke
                , Attr.strokeWidth <| String.fromFloat settings.lineWidth
                ]
                []
            ]

        ForeignObject str dim ->
            [ drawForeignObject withVerbatim settings pos dim str ]


opposite : Direction -> Direction
opposite dir =
    case dir of
        Center ->
            Center

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
            let
                oppositeDir =
                    Ext (opposite dir1) (opposite dir2)
            in
            oppositeDir

        Ext_ n dir1 dir2 ->
            let
                oppositeDir =
                    Ext_ n (opposite dir1) (opposite dir2)
            in
            oppositeDir


drawArrow : Settings -> Point -> Direction -> Svg msg
drawArrow settings pos dir =
    toLine
        [ Attr.style
            ("stroke: "
                ++ settings.color.stroke
                ++ ";stroke-width:"
                ++ String.fromFloat settings.lineWidth
            )
        , Attr.markerEnd "url(#triangle)"
        , vectorEffect
        ]
        (move dir pos)
        (opposite dir)


drawSquare : Bool -> Settings -> Point -> Svg msg
drawSquare big settings pos =
    let
        ( width, height ) =
            if big then
                ( settings.textWidth, settings.textHeight + 1 )

            else
                ( settings.textWidth, settings.textWidth )
    in
    Svg.rect
        [ Attr.x <| String.fromFloat (pos.x - width / 2)
        , Attr.y <| String.fromFloat (pos.y - height / 2)
        , Attr.stroke settings.color.stroke
        , Attr.fill settings.color.stroke
        , Attr.width <| String.fromFloat width
        , Attr.height <| String.fromFloat height
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


drawLine : Settings -> Point -> Direction -> Svg msg
drawLine s =
    toLine
        [ Attr.stroke s.color.stroke
        , Attr.strokeWidth <| String.fromFloat s.lineWidth
        , Attr.strokeLinecap "round"
        , Attr.strokeLinejoin "mitter"
        , vectorEffect
        ]


drawText : Settings -> Point -> String -> Svg msg
drawText s pos str =
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
                ++ "px;font-family:monospace;"
            )
        , Attr.fill s.color.text
        ]
        [ Svg.text str ]


drawForeignObject : Maybe (String -> Svg msg) -> Settings -> Point -> ( Int, Int ) -> String -> Svg msg
drawForeignObject withVerbatim s pos ( rows, columns ) str =
    case withVerbatim of
        Nothing ->
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
                , Attr.fill s.color.text
                ]
                [ Svg.text str ]

        Just verbatim ->
            drawCustomObject verbatim s pos ( rows, columns ) str


drawCustomObject : (a -> Svg msg) -> Settings -> Point -> ( Int, Int ) -> a -> Svg msg
drawCustomObject verbatim s pos ( rows, columns ) obj =
    let
        pos2 =
            move (Ext (North_ 1.1) West) pos
    in
    Svg.foreignObject
        [ Attr.x <| String.fromFloat pos2.x
        , Attr.y <| String.fromFloat pos2.y
        , s.verbatim.width
            |> Maybe.withDefault (String.fromFloat (1 + measureX columns))
            |> Attr.width
        , s.verbatim.height
            |> Maybe.withDefault (String.fromFloat (measureY rows))
            |> Attr.height
        , Attr.style
            ("font-size:"
                ++ String.fromFloat s.fontSize
                ++ "px;font-family:monospace"
            )
        , Attr.fill s.color.text
        ]
        [ verbatim obj ]


measureX : Int -> Float
measureX x =
    toFloat x * textWidth


measureY : Int -> Float
measureY y =
    toFloat y * textHeight


get : ( Int, Int ) -> Dict ( Int, Int ) ( String, Scan ) -> Scan
get pos dict =
    dict
        |> Dict.get pos
        |> Maybe.map Tuple.second
        |> Maybe.withDefault None
