module SvgBob.Grid exposing
    ( drawElements
    , getElements
    )

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import String
import Svg exposing (Svg)
import Svg.Attributes as Attr
import SvgBob.Model
    exposing
        ( Settings
        , init
        )
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
            Ext (moveExt n dir1) (moveExt n dir2)

        Ext_ m dir1 dir2 ->
            Ext (moveExt (n * m) dir1) (moveExt (n * m) dir2)


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


getMatrix : Int -> Int -> Int -> Dict Int ( String, Scan ) -> Matrix
getMatrix stride x y dict =
    let
        getNeighbor nx ny =
            get stride nx ny dict
    in
    { north_west = getNeighbor (x - 1) (y - 1)
    , north = getNeighbor x (y - 1)
    , north_east = getNeighbor (x + 1) (y - 1)
    , west = getNeighbor (x - 1) y
    , east = getNeighbor (x + 1) y
    , south_west = getNeighbor (x - 1) (y + 1)
    , south = getNeighbor x (y + 1)
    , south_east = getNeighbor (x + 1) (y + 1)
    }


textWidth : Float
textWidth =
    8.0


textHeight : Float
textHeight =
    16.0


apply : Matrix -> List ( Matrix -> Bool, Element ) -> List Element
apply matrix list =
    list
        |> List.filterMap
            (\( if_, then_ ) ->
                if if_ matrix then
                    Just then_

                else
                    Nothing
            )


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
    case list of
        [] ->
            Text char

        _ ->
            Sequence list


intersection : String -> Matrix -> Element
intersection char matrix =
    [ ( \{ north } -> north == Vertical || north == Intersection || north == Corner South
      , Line Center North
      )
    , ( \m -> m.south == Vertical || m.south == Intersection || m.south == Corner North
      , Line Center South
      )
    , ( .east >> (==) Horizontal, Line Center East )
    , ( .west >> (==) Horizontal, Line Center West )

    -- resistor caps `.+.` and `'+'`: the corner beside continues vertically
    , ( \m -> (Corner South == m.east && Vertical == m.south_east) || (Corner North == m.east && Vertical == m.north_east)
      , Line Center East
      )
    , ( \m -> (Corner South == m.west && Vertical == m.south_west) || (Corner North == m.west && Vertical == m.north_west)
      , Line Center West
      )
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
    [ ( \m -> Corner South == m.north_west || Corner North == m.south_west
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
    [ ( \m -> Corner South == m.north_east || Corner North == m.south_east
      , Curve 4 North (Ext South South)
      )
    , ( \m -> SlantRight == m.north_east && SlantLeft == m.south_east
      , Curve 4 (Ext North East) (Ext South South)
      )
    ]
        |> apply matrix
        |> sequenceWithDefault char


{-| a vertical line ends at this corner from the north
-}
fromNorth : Matrix -> Bool
fromNorth m =
    Vertical == m.north || Corner South == m.north || Intersection == m.north


{-| a vertical line ends at this corner from the south
-}
fromSouth : Matrix -> Bool
fromSouth m =
    Vertical == m.south || Corner North == m.south || Intersection == m.south


{-| a horizontal line ends at this corner, `+` included for resistors like `.+.`
-}
horizontalEnd : Scan -> Bool
horizontalEnd scan =
    Horizontal == scan || Intersection == scan


corner : Direction -> String -> Matrix -> Element
corner dir char matrix =
    [ ( \m -> (Horizontal == m.west) && (Horizontal == m.east)
      , Line West (East_ 2)
      )
    , ( \m -> (Vertical == m.north) && (Vertical == m.south)
      , Line North (South_ 2)
      )
    , ( \m -> (SlantRight == m.north_east) && (SlantRight == m.south_west)
      , Line (Ext North East) (Ext_ 2 South West)
      )
    , ( \m -> (SlantLeft == m.north_west) && (SlantLeft == m.south_east)
      , Line (Ext North West) (Ext_ 2 South East)
      )
    , ( \m -> fromNorth m && horizontalEnd m.west
      , Sequence
            [ Curve 1 West (Ext (North_ 0.5) East)
            , Line North (South_ 0.5)
            ]
      )
    , ( \m -> fromNorth m && horizontalEnd m.east
      , Sequence
            [ Curve 1 (North_ 0.5) (Ext (South_ 0.5) East)
            , Line North (South_ 0.5)
            ]
      )
    , ( \m -> fromSouth m && horizontalEnd m.west
      , Sequence
            [ Curve 1 (South_ 0.5) (Ext (North_ 0.5) West)
            , Line South (North_ 0.5)
            ]
      )
    , ( \m -> fromNorth m && (LowHorizontal == m.west)
      , Sequence
            [ Curve 1 (Ext South West) (Ext (North_ 0.5) East)
            , Line North (South_ 1.5)
            ]
      )
    , ( \m -> fromSouth m && (LowHorizontal == m.west)
      , Curve 1 (South_ 1.5) (Ext (North_ 0.5) West)
      )
    , ( \m -> fromSouth m && (LowHorizontal == m.east)
      , Curve 1 (Ext South East) (Ext (South_ 0.5) West)
      )
    , ( \m -> fromNorth m && (LowHorizontal == m.east)
      , Sequence
            [ Curve 1 (South_ 0.5) (Ext (South_ 0.5) East)
            , Line North (South_ 1.5)
            ]
      )
    , ( \m -> fromSouth m && horizontalEnd m.east
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
      , Curve 3 (Ext North West) (Ext South (East_ 2))
      )
    , ( \m -> (SlantRight == m.north) && (Horizontal == m.east)
      , Curve 2 (Ext North West) (Ext South (East_ 2))
      )
    , ( \m -> (SlantLeft == m.north) && (Horizontal == m.west)
      , Curve 2 (Ext North East) (Ext South (West_ 2))
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

    -- side vertices of a hexagon `>` and `<`, in a star the straight slants above already pass
    , ( \m -> SlantLeft == m.north_west && SlantRight == m.south_west && SlantRight /= m.north_east && SlantLeft /= m.south_east
      , Sequence [ Line Center (Ext North West), Line Center (Ext South West) ]
      )
    , ( \m -> SlantRight == m.north_east && SlantLeft == m.south_east && SlantRight /= m.south_west && SlantLeft /= m.north_west
      , Sequence [ Line Center (Ext North East), Line Center (Ext South East) ]
      )
    , ( \m -> m.west == LowHorizontal && m.east == Horizontal
      , Line (Ext South West) (Ext North (East_ 2))
      )
    , ( \m -> m.west == Horizontal && m.east == LowHorizontal
      , Line (Ext South East) (Ext North (West_ 2))
      )
    , ( \m -> m.west == Horizontal && m.north_east == LowHorizontal && not (fromNorth m || fromSouth m)
      , Line West (Ext North (East_ 2))
      )
    , ( \m -> m.east == Horizontal && m.north_west == LowHorizontal && not (fromNorth m || fromSouth m)
      , Line East (Ext North (West_ 2))
      )

    -- corner combinations
    , ( \m -> dir == North && m.west == Corner North
      , Line (Ext (North_ 0.5) West) East
      )
    , ( \m -> dir == North && m.east == Corner North
      , Line (Ext (North_ 0.5) East) West
      )
    , ( \m -> dir == South && m.west == Corner South
      , Line (Ext (South_ 0.5) West) East
      )
    , ( \m -> dir == South && m.east == Corner South
      , Line (Ext (South_ 0.5) East) West
      )

    -----
    , ( \m -> dir == South && m.east == Corner North
      , Line (South_ 0.5) (Ext (North_ 0.5) East)
      )
    , ( \m -> dir == South && m.south_east == Corner North
      , Line (South_ 0.5) (Ext (South_ 0.5) East)
      )
    , ( \m -> dir == South && m.west == Corner North
      , Line (South_ 0.5) (Ext (North_ 0.5) West)
      )
    , ( \m -> dir == South && m.south_west == Corner North
      , Line (South_ 0.5) (Ext (South_ 0.5) West)
      )
    , ( \m -> dir == North && m.east == Corner South
      , Line (North_ 0.5) (Ext (South_ 0.5) East)
      )
    , ( \m -> dir == North && m.north_east == Corner South
      , Line (North_ 0.5) (Ext (North_ 0.5) East)
      )
    , ( \m -> dir == North && m.west == Corner South
      , Line (North_ 0.5) (Ext (South_ 0.5) West)
      )
    , ( \m -> dir == North && m.north_west == Corner South
      , Line (North_ 0.5) (Ext (North_ 0.5) West)
      )

    -- a step `,'` or `` `. `` continues into a slant or a low line, as in round shapes
    , ( \m -> dir == South && m.east == Corner North && (SlantRight == m.south_west || LowHorizontal == m.west)
      , Line (South_ 0.5) (Ext (South_ 0.5) West)
      )
    , ( \m -> dir == South && m.west == Corner North && (SlantLeft == m.south_east || LowHorizontal == m.east)
      , Line (South_ 0.5) (Ext (South_ 0.5) East)
      )
    , ( \m -> dir == North && m.west == Corner South && (SlantRight == m.north_east || LowHorizontal == m.north_east)
      , Line (North_ 0.5) (Ext (North_ 0.5) East)
      )
    , ( \m -> dir == North && m.east == Corner South && (SlantLeft == m.north_west || LowHorizontal == m.north_west)
      , Line (North_ 0.5) (Ext (North_ 0.5) West)
      )
    ]
        |> apply matrix
        |> (\list ->
                if List.isEmpty list then
                    -- only a horizontal line ends here, as in `'----'`
                    apply matrix
                        [ ( .west >> (==) Horizontal, Line West East )
                        , ( .east >> (==) Horizontal, Line East West )
                        ]

                else
                    list
           )
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
                -- `-` stops at its cell border, so reach out to it from the center
                case apply m [ ( .west >> (==) Horizontal, Line Center West ), ( .east >> (==) Horizontal, Line Center East ) ] of
                    [] ->
                        Line South (Ext North North)

                    stubs ->
                        Sequence (Line South (Ext North North) :: stubs)

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

        Arrow dir ->
            -- East or West: `>` needs its line on the west, `<` on the east, so `<5k>` stays text
            let
                ( lineSide, otherSide ) =
                    if dir == West then
                        ( m.west, m.east )

                    else
                        ( m.east, m.west )
            in
            if
                List.member lineSide [ Horizontal, Corner South, Corner North ]
                    -- second head of `->>-`, but not text like `<<<<`
                    || (lineSide == Arrow dir && otherSide == Horizontal)
            then
                Triangle dir

            else
                Text char

        Corner dir ->
            corner dir char m

        SlantRight ->
            if AlphaNumeric == m.west && AlphaNumeric == m.east then
                -- part of a word like `1/8`; ponytail: o, v, V, A scan as shapes, so `w/o` stays a line
                Text char

            else
                Line (Ext North East) (Ext_ 2 South West)

        SlantLeft ->
            if AlphaNumeric == m.west && AlphaNumeric == m.east then
                Text char

            else
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
                |> SvgBob.Model.toLines
                |> SvgBob.Model.dim
                |> ForeignObject str

        Emoji ->
            TextEmoji char

        _ ->
            Text char


{-| `~ : ! =` are lines only when their run ends at another line and touches no text, so `Note:`, `a == b` or `~ ~ ~` stay text.
-}
lineOrText : (Int -> Int -> ( String, Scan )) -> Int -> Int -> ( String, Scan ) -> ( String, Scan )
lineOrText at x y ( char, scan ) =
    let
        -- a vertical run needs a vertical continuation, so `:` inside `( : )` between `.-.` and `'-'` stays text
        isLine : ( String, Scan ) -> Bool
        isLine ( _, s ) =
            List.member s
                (if scan == Vertical then
                    [ Vertical, Intersection, Corner North, Corner South, Arrow North, Arrow South ]

                 else
                    [ Horizontal, Intersection, Corner North, Corner South, Arrow East, Arrow West ]
                )

        isText : Int -> Bool
        isText nx =
            Tuple.second (at nx y) == AlphaNumeric

        -- the first cell after the run of `char` in direction (dx, dy)
        endOfRun : Int -> Int -> Int -> Int -> ( String, Scan )
        endOfRun dx dy nx ny =
            if Tuple.first (at nx ny) == char then
                endOfRun dx dy (nx + dx) (ny + dy)

            else
                at nx ny

        connected : Bool
        connected =
            if scan == Vertical then
                isLine (endOfRun 0 -1 x (y - 1)) || isLine (endOfRun 0 1 x (y + 1))

            else
                isLine (endOfRun -1 0 (x - 1) y) || isLine (endOfRun 1 0 (x + 1) y)
    in
    if not (List.member char [ "~", ":", "!", "=" ]) || (connected && not (isText (x - 1)) && not (isText (x + 1))) then
        ( char, scan )

    else
        ( char, AlphaNumeric )


{-| `~ : !` draw their lines dashed, `=` doubles them.
-}
restyle : String -> Element -> Element
restyle char element =
    case element of
        Sequence list ->
            Sequence (List.map (restyle char) list)

        Line start stop ->
            if char == "=" then
                Sequence [ Line (Ext start (North_ 0.25)) stop, Line (Ext start (South_ 0.25)) stop ]

            else if List.member char [ "~", ":", "!" ] then
                DashedLine start stop

            else
                element

        _ ->
            element


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
        , Attr.fill "transparent"
        ]
        []


drawSvg :
    List (Svg.Attribute msg)
    ->
        { model
            | settings : Settings
            , columns : Int
            , rows : Int
        }
    -> List (Svg msg)
    -> Html msg
drawSvg attributes model body =
    let
        color =
            model.settings.color
    in
    Svg.svg
        (viewBox model.rows model.columns
            :: bgColor color.background color.stroke
            :: Attr.stroke color.stroke
            :: Attr.strokeWidth (String.fromFloat model.settings.lineWidth)
            :: Attr.fill color.stroke
            :: attributes
        )
        body


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
            draw config.settings

        fnCustom =
            drawCustomObject verbatim config.settings
    in
    List.concatMap fnSVG config.svg
        |> List.append (List.map (\( a, ( point, dim ) ) -> fnCustom point dim a) config.foreign)
        |> drawSvg attributes config


viewBox : Int -> Int -> Attribute msg
viewBox rows columns =
    ("0 0 "
        ++ (String.fromFloat <| measureX columns + 10)
        ++ " "
        ++ (String.fromFloat <| measureY rows + 10)
    )
        |> Attr.viewBox


bgColor : String -> String -> Svg.Attribute msg
bgColor background fill =
    Attr.style ("background-color:" ++ background ++ "; fill:" ++ fill ++ ";")


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

        stride =
            model.columns + 2

        toDict : List ( ( Int, Int ), ( String, Scan ) ) -> Dict Int ( String, Scan )
        toDict =
            List.map (\( ( x, y ), v ) -> ( y * stride + x, v )) >> Dict.fromList

        raw : Dict Int ( String, Scan )
        raw =
            toDict intermediate

        resolved : List ( ( Int, Int ), ( String, Scan ) )
        resolved =
            List.map (\( ( x, y ), v ) -> ( ( x, y ), lineOrText (\nx ny -> Dict.get (ny * stride + nx) raw |> Maybe.withDefault ( "", None )) x y v )) intermediate

        dict =
            toDict resolved
    in
    List.foldl
        (\( ( x, y ), ( char, element ) ) container ->
            let
                point =
                    Point
                        (measureX x + textWidth / 2)
                        (measureY y + textHeight / 2)
            in
            case getElement (getMatrix stride x y dict) ( char, element ) |> restyle char of
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
        resolved


draw : Settings -> ( Point, Element ) -> List (Svg msg)
draw settings ( pos, element ) =
    case element of
        Triangle dir ->
            [ drawArrow pos dir ]

        Text char ->
            [ drawText settings pos char ]

        TextEmoji char ->
            [ drawText settings (move (East_ 0.25) pos) char ]

        Line start stop ->
            [ drawLine (move start pos) stop ]

        DashedLine start stop ->
            -- butt caps and a period of 8 keep the dashes evenly spaced across cells
            [ toLine [ Attr.strokeDasharray "4 4" ] (move start pos) stop ]

        Curve factor start stop ->
            [ drawArc settings factor (move start pos) stop ]

        Sequence elements ->
            let
                fn =
                    Tuple.pair pos >> draw settings
            in
            List.concatMap fn elements

        Box ->
            [ drawSquare False pos ]

        BigBox ->
            [ drawSquare True pos ]

        Circle filled ->
            [ Svg.circle
                [ Attr.cx <| String.fromFloat pos.x
                , Attr.cy <| String.fromFloat pos.y
                , Attr.r <| String.fromFloat settings.arcRadius
                , Attr.fill <|
                    if filled then
                        "inherit"

                    else
                        settings.color.background
                ]
                []
            ]

        ForeignObject _ _ ->
            -- getElements moves these into the foreign list
            []


dirToAngle : Direction -> Float
dirToAngle dir =
    -- Slant lines move (textWidth/2, textHeight/2) = (4, 8), so the true
    -- diagonal angle is atan2(8, 4) = atan(2) ≈ 63.43°, not 45°.
    case dir of
        East ->
            0

        South ->
            90

        West ->
            180

        North ->
            -90

        Ext North East ->
            -63.43

        Ext South East ->
            63.43

        Ext South West ->
            116.57

        Ext North West ->
            -116.57

        _ ->
            0


drawArrow : Point -> Direction -> Svg msg
drawArrow pos dir =
    Svg.polygon
        [ Attr.points "0,0 8,-4 8,4"
        , Attr.transform
            ("translate("
                ++ String.fromFloat pos.x
                ++ ","
                ++ String.fromFloat pos.y
                ++ ") rotate("
                ++ String.fromFloat (dirToAngle dir)
                ++ ")"
            )
        ]
        []


drawSquare : Bool -> Point -> Svg msg
drawSquare big pos =
    let
        ( width, height ) =
            if big then
                ( textWidth, textHeight + 1 )

            else
                ( textWidth, textWidth )
    in
    Svg.rect
        [ Attr.x <| String.fromFloat (pos.x - width / 2)
        , Attr.y <| String.fromFloat (pos.y - height / 2)
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


drawLine : Point -> Direction -> Svg msg
drawLine =
    toLine
        [ Attr.strokeLinecap "round"
        , Attr.strokeLinejoin "miter"
        ]


{-| Text styling is set per element, a <style> inside the svg would apply to the whole page.
-}
drawText : Settings -> Point -> String -> Svg msg
drawText settings pos str =
    let
        pos2 =
            move (Ext (South_ 0.5) West) pos
    in
    Svg.node "text"
        [ Attr.x <| String.fromFloat pos2.x
        , Attr.y <| String.fromFloat pos2.y
        , Attr.fill settings.color.text
        , Attr.stroke "none"
        , Attr.fontSize <| String.fromFloat settings.fontSize
        , Attr.fontFamily "monospace"
        ]
        [ Svg.text str ]


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
        ]
        [ verbatim obj ]


measureX : Int -> Float
measureX x =
    toFloat x * textWidth


measureY : Int -> Float
measureY y =
    toFloat y * textHeight


get : Int -> Int -> Int -> Dict Int ( String, Scan ) -> Scan
get stride x y dict =
    dict
        |> Dict.get (y * stride + x)
        |> Maybe.map Tuple.second
        |> Maybe.withDefault None
