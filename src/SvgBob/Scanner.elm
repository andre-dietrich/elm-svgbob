module SvgBob.Scanner exposing (getScans)

import SvgBob.Types
    exposing
        ( Direction(..)
        , Scan(..)
        , Scans
        , mergeVerbatim
        )
import WChar


getScans :
    { string : String
    , multiline : Bool
    , height : Maybe String
    , width : Maybe String
    }
    -> List (List String)
    -> Scans
getScans verbatim lines =
    let
        elements =
            lines
                |> List.indexedMap (scanLine verbatim.string)
                |> List.concat
    in
    if verbatim.multiline then
        let
            ( verbs, scans ) =
                List.foldl
                    (\( pos, ( char, scan ) ) ( v, s ) ->
                        case scan of
                            Verbatim _ ->
                                ( ( pos, ( char, scan ) ) :: v, s )

                            _ ->
                                ( v, ( pos, ( char, scan ) ) :: s )
                    )
                    ( [], [] )
                    elements
        in
        verbs
            |> List.sortBy (Tuple.first >> Tuple.second)
            |> merge []
            -- solves overlay issue ...
            |> List.reverse
            |> List.append scans

    else
        elements


type alias State =
    { result : Scans
    , x : Int
    , verbatimCounter : Int
    , lastWasVerbatim : Bool

    -- the last cell was appended to the verbatim run at the head of result
    , inRun : Bool
    }


scanLine : String -> Int -> List String -> Scans
scanLine verbatim y =
    trimRight
        >> List.foldl
            (scanElement verbatim y)
            { result = [], x = 0, verbatimCounter = 0, lastWasVerbatim = False, inRun = False }
        >> .result


trimRight : List String -> List String
trimRight =
    List.foldr
        (\char rest ->
            if List.isEmpty rest && String.trim char == "" then
                []

            else
                char :: rest
        )
        []


scanElement : String -> Int -> String -> State -> State
scanElement verbatim y char scan =
    (\s -> { s | x = s.x + WChar.graphemeWidth char, lastWasVerbatim = char == verbatim }) <|
        if char == verbatim then
            -- start verbatim mode
            case ( scan.verbatimCounter, scan.lastWasVerbatim ) of
                ( 0, _ ) ->
                    { scan | verbatimCounter = 1 }

                ( 1, False ) ->
                    { scan | verbatimCounter = 0, inRun = False }

                ( 1, True ) ->
                    { scan | verbatimCounter = 2 }

                ( 2, True ) ->
                    { scan
                        | verbatimCounter = 0
                        , inRun = False
                        , result =
                            case ( scan.inRun, scan.result ) of
                                ( True, ( pos, ( _, Verbatim str ) ) :: xs ) ->
                                    -- drop the first closing quote, that was appended as text
                                    ( pos, ( " ", Verbatim (String.dropRight 1 str ++ "  ") ) ) :: xs

                                _ ->
                                    scan.result
                    }

                _ ->
                    { scan | result = appendToVerbatim y char scan, inRun = True }

        else if scan.verbatimCounter > 0 then
            { scan | result = appendToVerbatim y char scan, inRun = True }

        else
            case getScan char of
                Nothing ->
                    scan

                Just elem ->
                    { scan | result = ( ( scan.x, y ), ( char, elem ) ) :: scan.result }


getScan : String -> Maybe Scan
getScan char =
    case char of
        " " ->
            Nothing

        "-" ->
            Just Horizontal

        "~" ->
            Just Horizontal

        "=" ->
            Just Horizontal

        ":" ->
            Just Vertical

        "!" ->
            Just Vertical

        "_" ->
            Just LowHorizontal

        "+" ->
            Just Intersection

        "." ->
            Just (Corner South)

        "'" ->
            Just (Corner North)

        "," ->
            Just (Corner South)

        "`" ->
            Just (Corner North)

        "´" ->
            Just (Corner North)

        ">" ->
            Just <| Arrow West

        "<" ->
            Just <| Arrow East

        "V" ->
            Just <| Arrow South

        "v" ->
            Just <| Arrow South

        "^" ->
            Just <| Arrow North

        "A" ->
            Just <| Arrow North

        "/" ->
            Just SlantRight

        "\\" ->
            Just SlantLeft

        "(" ->
            Just OpenCurve

        ")" ->
            Just CloseCurve

        "|" ->
            Just Vertical

        "#" ->
            Just Square

        "O" ->
            Just <| O False

        "o" ->
            Just <| O False

        "*" ->
            Just <| O True

        _ ->
            if WChar.graphemeWidth char == 2 then
                Just Emoji

            else
                Just AlphaNumeric


appendToVerbatim : Int -> String -> State -> Scans
appendToVerbatim y char scan =
    case ( scan.inRun, scan.result ) of
        ( True, ( pos, ( _, Verbatim str ) ) :: xs ) ->
            ( pos, ( " ", Verbatim (str ++ char) ) ) :: xs

        _ ->
            ( ( scan.x + 1 - scan.verbatimCounter, y ), ( " ", Verbatim char ) ) :: scan.result


merge : Scans -> Scans -> Scans
merge combined verbs =
    case verbs of
        [] ->
            combined

        [ single ] ->
            single :: combined

        head :: tail ->
            let
                ( _, verb, newTail ) =
                    List.foldl
                        (\( ( x, y ) as pos, ( c, s ) as scan ) ( currentY, ( ( v_x, v_y ), ( _, v_s ) ) as v, rest ) ->
                            if x == v_x && currentY + 1 == y then
                                -- the block keeps the position of its first row
                                ( currentY + 1
                                , ( ( v_x, v_y ), ( c, mergeVerbatim v_s s ) )
                                , rest
                                )

                            else
                                ( currentY
                                , v
                                , ( pos, scan ) :: rest
                                )
                        )
                        ( head |> Tuple.first |> Tuple.second
                        , head
                        , []
                        )
                        tail
            in
            merge (verb :: combined) (List.reverse newTail)
