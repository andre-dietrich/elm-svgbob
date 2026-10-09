module SvgBobTest exposing (suite)

import Expect
import Svg
import Svg.Attributes as Attr
import SvgBob
import SvgBob.Model
import SvgBob.Scanner
import SvgBob.Types exposing (Direction(..), Element(..), Scan(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (attribute, tag)
import WChar


settingsWith : Bool -> SvgBob.Settings
settingsWith verbatimOn =
    let
        settings =
            SvgBob.defaultOptions

        verbatim =
            settings.verbatim
    in
    { settings
        | verbatim =
            { verbatim
                | string =
                    if verbatimOn then
                        "\""

                    else
                        ""
            }
    }


scans : Bool -> String -> List ( ( Int, Int ), ( String, Scan ) )
scans verbatimOn str =
    let
        settings =
            settingsWith verbatimOn
    in
    SvgBob.Model.init settings str
        |> .lines
        |> SvgBob.Scanner.getScans settings.verbatim
        |> List.sortBy (\( ( x, y ), _ ) -> ( y, x ))


{-| x and grapheme of every non-verbatim cell in the first row
-}
cells : Bool -> String -> List ( Int, String )
cells verbatimOn =
    scans verbatimOn
        >> List.filterMap
            (\( ( x, y ), ( char, scan ) ) ->
                case scan of
                    Verbatim _ ->
                        Nothing

                    _ ->
                        if y == 0 then
                            Just ( x, char )

                        else
                            Nothing
            )


verbatims : String -> List ( Int, String )
verbatims =
    scans True
        >> List.filterMap
            (\( ( x, _ ), ( _, scan ) ) ->
                case scan of
                    Verbatim str ->
                        Just ( x, str )

                    _ ->
                        Nothing
            )


render : SvgBob.Settings -> String -> Query.Single msg
render settings str =
    SvgBob.getSvg [] str settings
        |> Query.fromHtml


elements : String -> List Element
elements str =
    (SvgBob.getElements SvgBob.defaultOptions str).svg
        |> List.sortBy (\( p, _ ) -> ( p.y, p.x ))
        |> List.map Tuple.second


unique : List Element -> Bool
unique list =
    case list of
        [] ->
            True

        x :: xs ->
            not (List.member x xs) && unique xs


noDuplicateParts : Element -> Bool
noDuplicateParts element =
    case element of
        Sequence list ->
            unique list && List.all noDuplicateParts list

        _ ->
            True


suite : Test
suite =
    describe "SvgBob"
        [ describe "WChar.graphemeWidth"
            ([ ( "a", 1 )
             , ( "\t", 1 )
             , ( "é", 1 )
             , ( "日", 2 )
             , ( "😀", 2 )
             , ( "❤️", 2 )
             , ( "🫠", 2 )
             ]
                |> List.map
                    (\( char, width ) ->
                        test (String.replace "\t" "tab" char) <| \_ -> Expect.equal width (WChar.graphemeWidth char)
                    )
            )
        , describe "columns fit all scanned cells"
            ([ String.repeat 5 "é", "❤️-->", "a\tbc", "😀-->", "🫠-->", "日本 -->", "\"日本\" -->", "\"a😀b\" -->" ]
                |> List.concatMap (\str -> [ ( str, False ), ( str, True ) ])
                |> List.map
                    (\( str, verbatimOn ) ->
                        test
                            (if verbatimOn then
                                str ++ " (verbatim)"

                             else
                                str
                            )
                        <|
                            \_ ->
                                scans verbatimOn str
                                    |> List.map (\( ( x, _ ), ( char, _ ) ) -> x + WChar.graphemeWidth char)
                                    |> List.maximum
                                    |> Maybe.withDefault 0
                                    |> Expect.atMost (SvgBob.Model.init (settingsWith verbatimOn) str).columns
                    )
            )
        , describe "scanner positions"
            [ test "combining accents take one cell" <|
                \_ ->
                    cells False (String.repeat 3 "é")
                        |> List.map Tuple.first
                        |> Expect.equal [ 0, 1, 2 ]
            , test "emoji presentation takes two cells" <|
                \_ ->
                    cells False "❤️->"
                        |> Expect.equal [ ( 0, "❤️" ), ( 2, "-" ), ( 3, ">" ) ]
            , test "wide characters inside verbatim keep the rest of the line in place" <|
                \_ ->
                    cells True "\"日本\" -->"
                        |> Expect.equal (List.filter (\( x, _ ) -> x > 5) (cells False "\"日本\" -->"))
            , test "emoji does not split a verbatim run" <|
                \_ ->
                    verbatims "\"a😀b\" -->"
                        |> Expect.equal [ ( 1, "a😀b" ) ]
            , test "two verbatim runs on one line" <|
                \_ ->
                    verbatims "\"ab\" \"cd\" -->"
                        |> Expect.equal [ ( 1, "ab" ), ( 6, "cd" ) ]
            , test "double quoted verbatim keeps literal quotes" <|
                \_ ->
                    verbatims "\"\"say \"hi\"\"\" x"
                        |> Expect.equal [ ( 1, "say \"hi  " ), ( 12, " x" ) ]
            , test "unclosed verbatim runs to the end of the line" <|
                \_ ->
                    verbatims "-- \"ab"
                        |> Expect.equal [ ( 4, "ab" ) ]
            ]
        , describe "rendering"
            [ test "no global <style> that leaks into the page" <|
                \_ ->
                    render SvgBob.defaultOptions "a -- o"
                        |> Query.findAll [ tag "style" ]
                        |> Query.count (Expect.equal 0)
            , test "text styling is set on each text element" <|
                \_ ->
                    render (SvgBob.setColors { stroke = "red", text = "green", background = "blue" } SvgBob.defaultOptions) "ab"
                        |> Query.findAll [ tag "text" ]
                        |> Query.each
                            (Query.has
                                [ attribute (Attr.fill "green")
                                , attribute (Attr.fontSize "14")
                                , attribute (Attr.fontFamily "monospace")
                                ]
                            )
            , test "open circles are filled with the background color" <|
                \_ ->
                    render (SvgBob.setColors { stroke = "red", text = "green", background = "blue" } SvgBob.defaultOptions) "-o-"
                        |> Query.find [ tag "circle" ]
                        |> Query.has [ attribute (Attr.fill "blue") ]
            , test "getSvg renders verbatim as plain text" <|
                \_ ->
                    render SvgBob.defaultOptions "\"ab\" -->"
                        |> Query.findAll [ tag "foreignObject" ]
                        |> Query.count (Expect.equal 0)
            , test "getSvgWith renders verbatim through the user function" <|
                \_ ->
                    SvgBob.getSvgWith (\s -> Svg.text ("<" ++ s ++ ">")) [] "\"ab\" -->" SvgBob.defaultOptions
                        |> Query.fromHtml
                        |> Query.find [ tag "foreignObject" ]
                        |> Query.has [ Selector.text "<ab>" ]
            , test "squares use the grid size, not settings.textWidth" <|
                \_ ->
                    let
                        settings =
                            SvgBob.defaultOptions
                    in
                    render { settings | textWidth = 12, textHeight = 20 } "-#-"
                        |> Query.find [ tag "rect" ]
                        |> Query.has [ attribute (Attr.width "8"), attribute (Attr.height "8") ]
            , test "big squares use the grid size" <|
                \_ ->
                    let
                        settings =
                            SvgBob.defaultOptions
                    in
                    render { settings | textWidth = 12, textHeight = 20 } "+#"
                        |> Query.find [ tag "rect" ]
                        |> Query.has [ attribute (Attr.width "8"), attribute (Attr.height "17") ]
            , test "dashed lines get a dash pattern" <|
                \_ ->
                    render SvgBob.defaultOptions "+~~+"
                        |> Query.findAll [ attribute (Attr.strokeDasharray "4 4") ]
                        |> Query.count (Expect.equal 2)
            , describe "arrow heads point along the line"
                ([ ( "-->", "translate(20,8) rotate(180)" )
                 , ( "<--", "translate(4,8) rotate(0)" )
                 , ( "|\nv", "translate(4,24) rotate(-90)" )
                 , ( "^\n|", "translate(4,8) rotate(90)" )
                 ]
                    |> List.map
                        (\( str, transform ) ->
                            test str <|
                                \_ ->
                                    render SvgBob.defaultOptions str
                                        |> Query.find [ tag "polygon" ]
                                        |> Query.has [ attribute (Attr.points "0,0 8,-4 8,4"), attribute (Attr.transform transform) ]
                        )
                )
            ]
        , describe "elements"
            [ test "arrow without a line is text" <|
                \_ -> Expect.equal [ Text ">" ] (elements ">")
            , test "right arrow" <|
                \_ -> Expect.equal (Just (Triangle West)) (List.head (List.reverse (elements "-->")))
            , test "left arrow" <|
                \_ -> Expect.equal (Just (Triangle East)) (List.head (elements "<--"))
            , test "rounded corner from east to south" <|
                \_ ->
                    elements ".-\n|"
                        |> List.head
                        |> Expect.equal (Just (Sequence [ Sequence [ Curve 1 East (Ext (South_ 0.5) West), Line South (North_ 0.5) ] ]))
            , test "rounded corner from north to east" <|
                \_ ->
                    elements "|\n'-"
                        |> List.drop 1
                        |> List.head
                        |> Expect.equal (Just (Sequence [ Sequence [ Curve 1 (North_ 0.5) (Ext (South_ 0.5) East), Line North (South_ 0.5) ] ]))
            , test "vertical line reaches a horizontal neighbour" <|
                \_ -> Expect.equal [ Sequence [ Line South (Ext North North), Line Center East ] ] (List.take 1 (elements "|-"))
            , test "arrow heads only on the line side, `<5k>` is a label" <|
                \_ -> Expect.equal [ Text "<", Text ">" ] (List.filter (\e -> e == Text "<" || e == Text ">") (elements "-<5k>-"))
            , test "double arrow head inside a line" <|
                \_ -> Expect.equal [ Triangle West, Triangle West ] (List.filter ((==) (Triangle West)) (elements "->>-"))
            , test "a row of arrow characters is text" <|
                \_ -> Expect.equal [ Text "<", Text "<", Text "<" ] (elements "<<<")
            , test "slash inside a word is text" <|
                \_ -> Expect.equal (Just (Text "/")) (List.head (List.drop 1 (elements "1/8")))
            , test "resistor cap `.+.` connects the plus to both corners" <|
                \_ -> Expect.equal (Just (Sequence [ Line Center East, Line Center West ])) (List.head (List.drop 1 (elements ".+.\n| |")))
            , test "side vertex of a hexagon" <|
                \_ ->
                    elements " /\n.\n \\"
                        |> List.drop 1
                        |> List.head
                        |> Expect.equal (Just (Sequence [ Sequence [ Line Center (Ext North East), Line Center (Ext South East) ] ]))
            , test "step of a round shape continues into the slant" <|
                \_ ->
                    elements " ,'\n/"
                        |> List.head
                        |> Expect.equal (Just (Sequence [ Line (South_ 0.5) (Ext (North_ 0.5) East), Line (South_ 0.5) (Ext (South_ 0.5) West) ]))
            , test "corner that only ends a horizontal line" <|
                \_ -> Expect.equal (Just (Sequence [ Line West East ])) (List.head (List.drop 1 (elements "-'")))
            , test "corner with a vertical leg ignores a diagonal low line" <|
                \_ ->
                    elements "_\n .-\n |"
                        |> List.drop 1
                        |> List.head
                        |> Expect.equal (Just (Sequence [ Sequence [ Curve 1 East (Ext (South_ 0.5) West), Line South (North_ 0.5) ] ]))
            , test "tilde continues a line dashed" <|
                \_ -> Expect.equal (Just (Sequence [ DashedLine East (West_ 2) ])) (List.head (List.drop 1 (elements "+~~+")))
            , test "colon and bang continue a vertical line dashed" <|
                \_ -> Expect.equal [ DashedLine South (Ext North North), DashedLine South (Ext North North) ] (List.take 2 (List.drop 1 (elements "+\n:\n!\n+")))
            , test "equal sign is a double line" <|
                \_ ->
                    Expect.equal (Just (Sequence [ Sequence [ Line (Ext East (North_ 0.25)) (West_ 2), Line (Ext East (South_ 0.25)) (West_ 2) ] ]))
                        (List.head (List.drop 1 (elements "+==+")))
            , describe "`~ : ! =` in text stay text"
                ([ "Note: |", "a = b", "~5 min", "Hi!\n|", "a == b", "~ ~ ~", "~~~~", " .-.\n( : )\n `-'" ]
                    |> List.map
                        (\str ->
                            test str <|
                                \_ ->
                                    elements str
                                        |> List.filter (\e -> List.member e (List.map Text [ ":", "=", "~", "!" ]))
                                        |> List.length
                                        |> Expect.equal (String.length (String.filter (\c -> List.member c [ ':', '=', '~', '!' ]) str))
                        )
                )
            , describe "nothing is drawn twice"
                ([ "-.\n  )"
                 , ".---.  .--->\n|   |  |\n'-+-'  '--( )\n  |  /  \\\n  v\n"
                 , "  ,-.   _\n (   ) | |\n  `-'  |_|\n +--#--+\n o  *  O"
                 , "\\|/\n.\n/|\\"
                 , "   ___\n ,'   `.\n/       \\\n\\       /\n `.___,'"
                 , "  ____\n /    \\\n. text .\n \\____/"
                 ]
                    |> List.map
                        (\str ->
                            test str <|
                                \_ ->
                                    elements str
                                        |> List.all noDuplicateParts
                                        |> Expect.equal True
                        )
                )
            ]
        ]
