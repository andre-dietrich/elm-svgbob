module SvgBob.Scanner exposing (getScans)

import Dict exposing (Dict)
import String.Graphemes
import SvgBob.Types
    exposing
        ( Direction(..)
        , Scan(..)
        , Scans
        , mergeVerbatim
        )


getScans :
    { string : String
    , multiline : Bool
    , height : Maybe String
    , width : Maybe String
    }
    -> List String
    -> Scans
getScans verbatim lines =
    let
        scanFn =
            scanLine
                verbatim.string
                (verbatim.string /= "")

        elements =
            lines
                |> List.indexedMap scanFn
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


scanLine : String -> Bool -> Int -> String -> Scans
scanLine verbatim withVerbatim y =
    String.trimRight
        >> String.Graphemes.toList
        >> List.foldl
            (scanElement verbatim withVerbatim y)
            { result = [], x = 0, verbatimCounter = 0, lastChars = [] }
        >> .result


scanElement :
    String
    -> Bool
    -> Int
    -> String
    -> { result : Scans, x : Int, verbatimCounter : Int, lastChars : List Bool }
    -> { result : Scans, x : Int, verbatimCounter : Int, lastChars : List Bool }
scanElement verbatim withVerbatim y char scan =
    (\s -> { s | x = s.x + 1, lastChars = (char == verbatim) :: s.lastChars }) <|
        if char == verbatim then
            -- start verbatim mode
            case ( scan.verbatimCounter, scan.lastChars ) of
                ( 0, _ ) ->
                    { scan | verbatimCounter = 1 }

                ( 1, False :: _ ) ->
                    { scan | verbatimCounter = 0 }

                ( 1, True :: _ ) ->
                    { scan | verbatimCounter = 2 }

                ( 2, True :: _ ) ->
                    { scan
                        | verbatimCounter = 0
                        , result =
                            case ( withVerbatim, scan.result ) of
                                ( True, ( pos, ( _, Verbatim str ) ) :: xs ) ->
                                    ( pos
                                    , ( " "
                                      , String.dropRight 1 str
                                            ++ "  "
                                            |> Verbatim
                                      )
                                    )
                                        :: xs

                                ( _, result ) ->
                                    result |> List.tail |> Maybe.withDefault []
                    }

                _ ->
                    { scan
                        | result =
                            case ( withVerbatim, scan.result ) of
                                ( False, _ ) ->
                                    ( ( scan.x, y ), ( char, AlphaNumeric ) ) :: scan.result

                                ( True, ( pos, ( _, Verbatim str ) ) :: xs ) ->
                                    ( pos, appendToVerbatim str char ) :: xs

                                ( True, _ ) ->
                                    ( ( scan.x, y ), appendToVerbatim "" char ) :: scan.result
                    }

        else if scan.verbatimCounter > 0 then
            { scan
                | result =
                    case ( withVerbatim, scan.result ) of
                        ( False, _ ) ->
                            ( ( scan.x, y ), ( char, AlphaNumeric ) ) :: scan.result

                        ( True, ( ( x, _ ), ( _, Verbatim str ) ) :: xs ) ->
                            if x + String.length str == scan.x then
                                ( ( x, y ), appendToVerbatim str char ) :: xs

                            else
                                ( ( scan.x, y ), appendToVerbatim "" char ) :: scan.result

                        ( True, _ ) ->
                            ( ( scan.x + 1 - scan.verbatimCounter, y ), appendToVerbatim "" char ) :: scan.result
            }

        else
            case getScan char of
                Nothing ->
                    scan

                Just Emoji ->
                    { scan
                        | x = scan.x + 1
                        , result = ( ( scan.x, y ), ( char, Emoji ) ) :: scan.result
                    }

                Just elem ->
                    { scan
                        | result = ( ( scan.x, y ), ( char, elem ) ) :: scan.result
                    }


characterScans : Dict String Scan
characterScans =
    Dict.fromList
        [ ( "-", Horizontal )
        , ( "_", LowHorizontal )
        , ( "+", Intersection )
        , ( ".", Corner )
        , ( "'", Corner )
        , ( ",", Corner )
        , ( "`", Corner )
        , ( "´", Corner )
        , ( ">", Arrow West )
        , ( "<", Arrow East )
        , ( "V", Arrow South )
        , ( "v", Arrow South )
        , ( "^", Arrow North )
        , ( "A", Arrow North )
        , ( "/", SlantRight )
        , ( "\\", SlantLeft )
        , ( "(", OpenCurve )
        , ( ")", CloseCurve )
        , ( "|", Vertical )
        , ( "#", Square )
        , ( "O", O False )
        , ( "o", O False )
        , ( "*", O True )
        ]


getScan : String -> Maybe Scan
getScan char =
    if char == " " then
        Nothing

    else
        Dict.get char characterScans
            |> Maybe.withDefault
                (if String.length char == 1 && not (isEmoji char) then
                    AlphaNumeric

                 else
                    Emoji
                )
            |> Just


isEmoji : String -> Bool
isEmoji char =
    char
        |> String.uncons
        |> Maybe.map (Tuple.first >> Char.toCode)
        |> Maybe.withDefault 0
        |> isWideCharacter


{-| Function to check if a character is a wide character (takes more space)
-}
isWideCharacter : Int -> Bool
isWideCharacter codePoint =
    if codePoint < 0x2000 then
        False

    else if codePoint <= 0x0001FAFF then
        (codePoint <= 0x206F)
            || (0x2300 <= codePoint && codePoint <= 0x23FF)
            || (0x25A0 <= codePoint && codePoint <= 0x25FF)
            || (0x2600 <= codePoint && codePoint <= 0x27BF)
            || (0x2900 <= codePoint && codePoint <= 0x297F)
            || (0x2B50 <= codePoint && codePoint <= 0x2B55)
            || (0x3000 <= codePoint && codePoint <= 0x303F)
            || (0xFF00 <= codePoint && codePoint <= 0xFFEF)
            || (0x0001F000 <= codePoint)

    else
        False


appendToVerbatim : String -> String -> ( String, Scan )
appendToVerbatim str =
    String.append str >> Verbatim >> Tuple.pair " "


merge : Scans -> Scans -> Scans
merge combined verbs =
    case ( List.head verbs, List.tail verbs ) of
        ( Nothing, _ ) ->
            combined

        ( _, Nothing ) ->
            combined

        ( Just head, Just tail ) ->
            let
                ( _, verb, newTail ) =
                    tail
                        |> List.foldl
                            (\( ( x, y ) as pos, ( c, s ) as scan ) ( currentY, ( ( v_x, v_y ), ( _, v_s ) ) as v, rest ) ->
                                if x == v_x && currentY + 1 == y then
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
                            ( head
                                |> Tuple.first
                                |> Tuple.second
                            , head
                            , []
                            )
            in
            merge (verb :: combined) (List.reverse newTail)
