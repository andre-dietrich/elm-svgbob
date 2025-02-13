module WChar exposing (Width(..), stringWidth, width)

import Array exposing (Array)


{-| Size of unicode character.

  - `Narrow` - single width character such as alphabet
  - `Wide` - double width charcter such as CJK character, emojis
  - `Zero` - zero width character
  - `Control` - control character

-}
type
    Width
    -- actual size
    = Narrow -- 1
    | Wide -- 2
    | Zero -- 0
    | Control -- 0


{-| Given a unicode string, return its printable length on a terminal.

This function returns the width, in cells, necessary to display the unicode
string `str`. Returns `Nothing` if a non-printable character is encountered.

-}
stringWidth : String -> Maybe Int
stringWidth str =
    let
        toInt w =
            case w of
                Narrow ->
                    Just 1

                Wide ->
                    Just 2

                Zero ->
                    Just 0

                Control ->
                    Nothing
    in
    String.toList str
        |> List.map width
        |> List.map toInt
        |> List.foldl (Maybe.map2 (+)) (Just 0)


{-| Given one Unicode character, return its printable length on a terminal.

This function returns the width, in cells, necessary to display the character of
Unicode string character, `ucs`. Returns `Zero` if the `ucs` argument has
no printable effect on a terminal (such as NULL '\\0'), `Control` if `ucs` is
not printable, or has an indeterminate effect on the terminal, such as
a control character. Otherwise, the number of column positions the
character occupies on a graphic terminal (Narrow or Wide) is returned.

The following have a column width of **Control**:

  - C0 control characters (`U+001` through `U+01F`).
  - C1 control characters and DEL (`U+07F` through `U+0A0`).

The following have a column width of **Zero**:

  - Non-spacing and enclosing combining characters (general
    category code Mn or Me in the Unicode database).
  - NULL (`U+0000`).
  - COMBINING GRAPHEME JOINER (`U+034F`).
  - ZERO WIDTH SPACE (`U+200B`) _through_
    RIGHT-TO-LEFT MARK (`U+200F`).
  - LINE SEPARATOR (`U+2028`) _and_
    PARAGRAPH SEPARATOR (`U+2029`).
  - LEFT-TO-RIGHT EMBEDDING (`U+202A`) _through_
    RIGHT-TO-LEFT OVERRIDE (`U+202E`).
  - WORD JOINER (`U+2060`) _through_
    INVISIBLE SEPARATOR (`U+2063`).

The following have a column width of **Narrow**:

  - SOFT HYPHEN (`U+00AD`).
  - All remaining characters, including all printable ISO 8859-1
    and WGL4 characters, Unicode control characters, etc.

The following have a column width of **Wide**:

  - Spacing characters in the East Asian Wide (W) or East Asian
    Full-width (F) category as defined in Unicode Technical
    Report #11 have a column width of 2.
  - Some kinds of Emoji or symbols.

-}
width : Char -> Width
width ucs =
    let
        code =
            Char.toCode ucs
    in
    -- test for 8-bit control characters
    if code == 0 then
        Zero

    else if code < 32 || (code >= 0x7F && code < 0xA0) then
        Control
        -- binary search in table of non-spacing characters

    else if binarySearch code zeroWidth then
        Zero
        -- binary search in table of wide eastern character

    else if binarySearch code wideEastern then
        Wide

    else
        Narrow



--
-- Privates
--


type alias CharTable =
    Array ( Int, Int )


binarySearch : Int -> CharTable -> Bool
binarySearch ucs table =
    let
        min =
            0

        max =
            Array.length table
    in
    bisearchHelper ucs table min max


bisearchHelper : Int -> CharTable -> Int -> Int -> Bool
bisearchHelper ucs table min max =
    let
        mid =
            (min + max) // 2
    in
    case Array.get mid table of
        Just ( beginning, ending ) ->
            if max < min then
                False

            else if ucs > ending then
                bisearchHelper ucs table (mid + 1) max

            else if ucs < beginning then
                bisearchHelper ucs table min (mid - 1)

            else
                True

        Nothing ->
            False



-- Unicode version '13.0.0':
--   Source: DerivedGeneralCategory-13.0.0.txt
--   Date:  2019-10-21, 14:30:32 GMT
--


zeroWidth : CharTable
zeroWidth =
    Array.fromList
        [ ( 0x0300, 0x036F )
        , -- Combining Grave Accent  ..Combining Latin Small Le
          ( 0x0483, 0x0489 )
        , -- Combining Cyrillic Titlo..Combining Cyrillic Milli
          ( 0x0591, 0x05BD )
        , -- Hebrew Accent Etnahta   ..Hebrew Point Meteg
          ( 0x05BF, 0x05BF )
        , -- Hebrew Point Rafe       ..Hebrew Point Rafe
          ( 0x05C1, 0x05C2 )
        , -- Hebrew Point Shin Dot   ..Hebrew Point Sin Dot
          ( 0x05C4, 0x05C5 )
        , -- Hebrew Mark Upper Dot   ..Hebrew Mark Lower Dot
          ( 0x05C7, 0x05C7 )
        , -- Hebrew Point Qamats Qata..Hebrew Point Qamats Qata
          ( 0x0610, 0x061A )
        , -- Arabic Sign Sallallahou ..Arabic Small Kasra
          ( 0x064B, 0x065F )
        , -- Arabic Fathatan         ..Arabic Wavy Hamza Below
          ( 0x0670, 0x0670 )
        , -- Arabic Letter Superscrip..Arabic Letter Superscrip
          ( 0x06D6, 0x06DC )
        , -- Arabic Small High Ligatu..Arabic Small High Seen
          ( 0x06DF, 0x06E4 )
        , -- Arabic Small High Rounde..Arabic Small High Madda
          ( 0x06E7, 0x06E8 )
        , -- Arabic Small High Yeh   ..Arabic Small High Noon
          ( 0x06EA, 0x06ED )
        , -- Arabic Empty Centre Low ..Arabic Small Low Meem
          ( 0x0711, 0x0711 )
        , -- Syriac Letter Superscrip..Syriac Letter Superscrip
          ( 0x0730, 0x074A )
        , -- Syriac Pthaha Above     ..Syriac Barrekh
          ( 0x07A6, 0x07B0 )
        , -- Thaana Abafili          ..Thaana Sukun
          ( 0x07EB, 0x07F3 )
        , -- Nko Combining Short High..Nko Combining Double Dot
          ( 0x07FD, 0x07FD )
        , -- Nko Dantayalan          ..Nko Dantayalan
          ( 0x0816, 0x0819 )
        , -- Samaritan Mark In       ..Samaritan Mark Dagesh
          ( 0x081B, 0x0823 )
        , -- Samaritan Mark Epentheti..Samaritan Vowel Sign A
          ( 0x0825, 0x0827 )
        , -- Samaritan Vowel Sign Sho..Samaritan Vowel Sign U
          ( 0x0829, 0x082D )
        , -- Samaritan Vowel Sign Lon..Samaritan Mark Nequdaa
          ( 0x0859, 0x085B )
        , -- Mandaic Affrication Mark..Mandaic Gemination Mark
          ( 0x08D3, 0x08E1 )
        , -- Arabic Small Low Waw    ..Arabic Small High Sign S
          ( 0x08E3, 0x0902 )
        , -- Arabic Turned Damma Belo..Devanagari Sign Anusvara
          ( 0x093A, 0x093A )
        , -- Devanagari Vowel Sign Oe..Devanagari Vowel Sign Oe
          ( 0x093C, 0x093C )
        , -- Devanagari Sign Nukta   ..Devanagari Sign Nukta
          ( 0x0941, 0x0948 )
        , -- Devanagari Vowel Sign U ..Devanagari Vowel Sign Ai
          ( 0x094D, 0x094D )
        , -- Devanagari Sign Virama  ..Devanagari Sign Virama
          ( 0x0951, 0x0957 )
        , -- Devanagari Stress Sign U..Devanagari Vowel Sign Uu
          ( 0x0962, 0x0963 )
        , -- Devanagari Vowel Sign Vo..Devanagari Vowel Sign Vo
          ( 0x0981, 0x0981 )
        , -- Bengali Sign Candrabindu..Bengali Sign Candrabindu
          ( 0x09BC, 0x09BC )
        , -- Bengali Sign Nukta      ..Bengali Sign Nukta
          ( 0x09C1, 0x09C4 )
        , -- Bengali Vowel Sign U    ..Bengali Vowel Sign Vocal
          ( 0x09CD, 0x09CD )
        , -- Bengali Sign Virama     ..Bengali Sign Virama
          ( 0x09E2, 0x09E3 )
        , -- Bengali Vowel Sign Vocal..Bengali Vowel Sign Vocal
          ( 0x09FE, 0x09FE )
        , -- Bengali Sandhi Mark     ..Bengali Sandhi Mark
          ( 0x0A01, 0x0A02 )
        , -- Gurmukhi Sign Adak Bindi..Gurmukhi Sign Bindi
          ( 0x0A3C, 0x0A3C )
        , -- Gurmukhi Sign Nukta     ..Gurmukhi Sign Nukta
          ( 0x0A41, 0x0A42 )
        , -- Gurmukhi Vowel Sign U   ..Gurmukhi Vowel Sign Uu
          ( 0x0A47, 0x0A48 )
        , -- Gurmukhi Vowel Sign Ee  ..Gurmukhi Vowel Sign Ai
          ( 0x0A4B, 0x0A4D )
        , -- Gurmukhi Vowel Sign Oo  ..Gurmukhi Sign Virama
          ( 0x0A51, 0x0A51 )
        , -- Gurmukhi Sign Udaat     ..Gurmukhi Sign Udaat
          ( 0x0A70, 0x0A71 )
        , -- Gurmukhi Tippi          ..Gurmukhi Addak
          ( 0x0A75, 0x0A75 )
        , -- Gurmukhi Sign Yakash    ..Gurmukhi Sign Yakash
          ( 0x0A81, 0x0A82 )
        , -- Gujarati Sign Candrabind..Gujarati Sign Anusvara
          ( 0x0ABC, 0x0ABC )
        , -- Gujarati Sign Nukta     ..Gujarati Sign Nukta
          ( 0x0AC1, 0x0AC5 )
        , -- Gujarati Vowel Sign U   ..Gujarati Vowel Sign Cand
          ( 0x0AC7, 0x0AC8 )
        , -- Gujarati Vowel Sign E   ..Gujarati Vowel Sign Ai
          ( 0x0ACD, 0x0ACD )
        , -- Gujarati Sign Virama    ..Gujarati Sign Virama
          ( 0x0AE2, 0x0AE3 )
        , -- Gujarati Vowel Sign Voca..Gujarati Vowel Sign Voca
          ( 0x0AFA, 0x0AFF )
        , -- Gujarati Sign Sukun     ..Gujarati Sign Two-circle
          ( 0x0B01, 0x0B01 )
        , -- Oriya Sign Candrabindu  ..Oriya Sign Candrabindu
          ( 0x0B3C, 0x0B3C )
        , -- Oriya Sign Nukta        ..Oriya Sign Nukta
          ( 0x0B3F, 0x0B3F )
        , -- Oriya Vowel Sign I      ..Oriya Vowel Sign I
          ( 0x0B41, 0x0B44 )
        , -- Oriya Vowel Sign U      ..Oriya Vowel Sign Vocalic
          ( 0x0B4D, 0x0B4D )
        , -- Oriya Sign Virama       ..Oriya Sign Virama
          ( 0x0B55, 0x0B56 )
        , -- (nil)                   ..Oriya Ai Length Mark
          ( 0x0B62, 0x0B63 )
        , -- Oriya Vowel Sign Vocalic..Oriya Vowel Sign Vocalic
          ( 0x0B82, 0x0B82 )
        , -- Tamil Sign Anusvara     ..Tamil Sign Anusvara
          ( 0x0BC0, 0x0BC0 )
        , -- Tamil Vowel Sign Ii     ..Tamil Vowel Sign Ii
          ( 0x0BCD, 0x0BCD )
        , -- Tamil Sign Virama       ..Tamil Sign Virama
          ( 0x0C00, 0x0C00 )
        , -- Telugu Sign Combining Ca..Telugu Sign Combining Ca
          ( 0x0C04, 0x0C04 )
        , -- Telugu Sign Combining An..Telugu Sign Combining An
          ( 0x0C3E, 0x0C40 )
        , -- Telugu Vowel Sign Aa    ..Telugu Vowel Sign Ii
          ( 0x0C46, 0x0C48 )
        , -- Telugu Vowel Sign E     ..Telugu Vowel Sign Ai
          ( 0x0C4A, 0x0C4D )
        , -- Telugu Vowel Sign O     ..Telugu Sign Virama
          ( 0x0C55, 0x0C56 )
        , -- Telugu Length Mark      ..Telugu Ai Length Mark
          ( 0x0C62, 0x0C63 )
        , -- Telugu Vowel Sign Vocali..Telugu Vowel Sign Vocali
          ( 0x0C81, 0x0C81 )
        , -- Kannada Sign Candrabindu..Kannada Sign Candrabindu
          ( 0x0CBC, 0x0CBC )
        , -- Kannada Sign Nukta      ..Kannada Sign Nukta
          ( 0x0CBF, 0x0CBF )
        , -- Kannada Vowel Sign I    ..Kannada Vowel Sign I
          ( 0x0CC6, 0x0CC6 )
        , -- Kannada Vowel Sign E    ..Kannada Vowel Sign E
          ( 0x0CCC, 0x0CCD )
        , -- Kannada Vowel Sign Au   ..Kannada Sign Virama
          ( 0x0CE2, 0x0CE3 )
        , -- Kannada Vowel Sign Vocal..Kannada Vowel Sign Vocal
          ( 0x0D00, 0x0D01 )
        , -- Malayalam Sign Combining..Malayalam Sign Candrabin
          ( 0x0D3B, 0x0D3C )
        , -- Malayalam Sign Vertical ..Malayalam Sign Circular
          ( 0x0D41, 0x0D44 )
        , -- Malayalam Vowel Sign U  ..Malayalam Vowel Sign Voc
          ( 0x0D4D, 0x0D4D )
        , -- Malayalam Sign Virama   ..Malayalam Sign Virama
          ( 0x0D62, 0x0D63 )
        , -- Malayalam Vowel Sign Voc..Malayalam Vowel Sign Voc
          ( 0x0D81, 0x0D81 )
        , -- (nil)                   ..(nil)
          ( 0x0DCA, 0x0DCA )
        , -- Sinhala Sign Al-lakuna  ..Sinhala Sign Al-lakuna
          ( 0x0DD2, 0x0DD4 )
        , -- Sinhala Vowel Sign Ketti..Sinhala Vowel Sign Ketti
          ( 0x0DD6, 0x0DD6 )
        , -- Sinhala Vowel Sign Diga ..Sinhala Vowel Sign Diga
          ( 0x0E31, 0x0E31 )
        , -- Thai Character Mai Han-a..Thai Character Mai Han-a
          ( 0x0E34, 0x0E3A )
        , -- Thai Character Sara I   ..Thai Character Phinthu
          ( 0x0E47, 0x0E4E )
        , -- Thai Character Maitaikhu..Thai Character Yamakkan
          ( 0x0EB1, 0x0EB1 )
        , -- Lao Vowel Sign Mai Kan  ..Lao Vowel Sign Mai Kan
          ( 0x0EB4, 0x0EBC )
        , -- Lao Vowel Sign I        ..Lao Semivowel Sign Lo
          ( 0x0EC8, 0x0ECD )
        , -- Lao Tone Mai Ek         ..Lao Niggahita
          ( 0x0F18, 0x0F19 )
        , -- Tibetan Astrological Sig..Tibetan Astrological Sig
          ( 0x0F35, 0x0F35 )
        , -- Tibetan Mark Ngas Bzung ..Tibetan Mark Ngas Bzung
          ( 0x0F37, 0x0F37 )
        , -- Tibetan Mark Ngas Bzung ..Tibetan Mark Ngas Bzung
          ( 0x0F39, 0x0F39 )
        , -- Tibetan Mark Tsa -phru  ..Tibetan Mark Tsa -phru
          ( 0x0F71, 0x0F7E )
        , -- Tibetan Vowel Sign Aa   ..Tibetan Sign Rjes Su Nga
          ( 0x0F80, 0x0F84 )
        , -- Tibetan Vowel Sign Rever..Tibetan Mark Halanta
          ( 0x0F86, 0x0F87 )
        , -- Tibetan Sign Lci Rtags  ..Tibetan Sign Yang Rtags
          ( 0x0F8D, 0x0F97 )
        , -- Tibetan Subjoined Sign L..Tibetan Subjoined Letter
          ( 0x0F99, 0x0FBC )
        , -- Tibetan Subjoined Letter..Tibetan Subjoined Letter
          ( 0x0FC6, 0x0FC6 )
        , -- Tibetan Symbol Padma Gda..Tibetan Symbol Padma Gda
          ( 0x102D, 0x1030 )
        , -- Myanmar Vowel Sign I    ..Myanmar Vowel Sign Uu
          ( 0x1032, 0x1037 )
        , -- Myanmar Vowel Sign Ai   ..Myanmar Sign Dot Below
          ( 0x1039, 0x103A )
        , -- Myanmar Sign Virama     ..Myanmar Sign Asat
          ( 0x103D, 0x103E )
        , -- Myanmar Consonant Sign M..Myanmar Consonant Sign M
          ( 0x1058, 0x1059 )
        , -- Myanmar Vowel Sign Vocal..Myanmar Vowel Sign Vocal
          ( 0x105E, 0x1060 )
        , -- Myanmar Consonant Sign M..Myanmar Consonant Sign M
          ( 0x1071, 0x1074 )
        , -- Myanmar Vowel Sign Geba ..Myanmar Vowel Sign Kayah
          ( 0x1082, 0x1082 )
        , -- Myanmar Consonant Sign S..Myanmar Consonant Sign S
          ( 0x1085, 0x1086 )
        , -- Myanmar Vowel Sign Shan ..Myanmar Vowel Sign Shan
          ( 0x108D, 0x108D )
        , -- Myanmar Sign Shan Counci..Myanmar Sign Shan Counci
          ( 0x109D, 0x109D )
        , -- Myanmar Vowel Sign Aiton..Myanmar Vowel Sign Aiton
          ( 0x135D, 0x135F )
        , -- Ethiopic Combining Gemin..Ethiopic Combining Gemin
          ( 0x1712, 0x1714 )
        , -- Tagalog Vowel Sign I    ..Tagalog Sign Virama
          ( 0x1732, 0x1734 )
        , -- Hanunoo Vowel Sign I    ..Hanunoo Sign Pamudpod
          ( 0x1752, 0x1753 )
        , -- Buhid Vowel Sign I      ..Buhid Vowel Sign U
          ( 0x1772, 0x1773 )
        , -- Tagbanwa Vowel Sign I   ..Tagbanwa Vowel Sign U
          ( 0x17B4, 0x17B5 )
        , -- Khmer Vowel Inherent Aq ..Khmer Vowel Inherent Aa
          ( 0x17B7, 0x17BD )
        , -- Khmer Vowel Sign I      ..Khmer Vowel Sign Ua
          ( 0x17C6, 0x17C6 )
        , -- Khmer Sign Nikahit      ..Khmer Sign Nikahit
          ( 0x17C9, 0x17D3 )
        , -- Khmer Sign Muusikatoan  ..Khmer Sign Bathamasat
          ( 0x17DD, 0x17DD )
        , -- Khmer Sign Atthacan     ..Khmer Sign Atthacan
          ( 0x180B, 0x180D )
        , -- Mongolian Free Variation..Mongolian Free Variation
          ( 0x1885, 0x1886 )
        , -- Mongolian Letter Ali Gal..Mongolian Letter Ali Gal
          ( 0x18A9, 0x18A9 )
        , -- Mongolian Letter Ali Gal..Mongolian Letter Ali Gal
          ( 0x1920, 0x1922 )
        , -- Limbu Vowel Sign A      ..Limbu Vowel Sign U
          ( 0x1927, 0x1928 )
        , -- Limbu Vowel Sign E      ..Limbu Vowel Sign O
          ( 0x1932, 0x1932 )
        , -- Limbu Small Letter Anusv..Limbu Small Letter Anusv
          ( 0x1939, 0x193B )
        , -- Limbu Sign Mukphreng    ..Limbu Sign Sa-i
          ( 0x1A17, 0x1A18 )
        , -- Buginese Vowel Sign I   ..Buginese Vowel Sign U
          ( 0x1A1B, 0x1A1B )
        , -- Buginese Vowel Sign Ae  ..Buginese Vowel Sign Ae
          ( 0x1A56, 0x1A56 )
        , -- Tai Tham Consonant Sign ..Tai Tham Consonant Sign
          ( 0x1A58, 0x1A5E )
        , -- Tai Tham Sign Mai Kang L..Tai Tham Consonant Sign
          ( 0x1A60, 0x1A60 )
        , -- Tai Tham Sign Sakot     ..Tai Tham Sign Sakot
          ( 0x1A62, 0x1A62 )
        , -- Tai Tham Vowel Sign Mai ..Tai Tham Vowel Sign Mai
          ( 0x1A65, 0x1A6C )
        , -- Tai Tham Vowel Sign I   ..Tai Tham Vowel Sign Oa B
          ( 0x1A73, 0x1A7C )
        , -- Tai Tham Vowel Sign Oa A..Tai Tham Sign Khuen-lue
          ( 0x1A7F, 0x1A7F )
        , -- Tai Tham Combining Crypt..Tai Tham Combining Crypt
          ( 0x1AB0, 0x1AC0 )
        , -- Combining Doubled Circum..(nil)
          ( 0x1B00, 0x1B03 )
        , -- Balinese Sign Ulu Ricem ..Balinese Sign Surang
          ( 0x1B34, 0x1B34 )
        , -- Balinese Sign Rerekan   ..Balinese Sign Rerekan
          ( 0x1B36, 0x1B3A )
        , -- Balinese Vowel Sign Ulu ..Balinese Vowel Sign Ra R
          ( 0x1B3C, 0x1B3C )
        , -- Balinese Vowel Sign La L..Balinese Vowel Sign La L
          ( 0x1B42, 0x1B42 )
        , -- Balinese Vowel Sign Pepe..Balinese Vowel Sign Pepe
          ( 0x1B6B, 0x1B73 )
        , -- Balinese Musical Symbol ..Balinese Musical Symbol
          ( 0x1B80, 0x1B81 )
        , -- Sundanese Sign Panyecek ..Sundanese Sign Panglayar
          ( 0x1BA2, 0x1BA5 )
        , -- Sundanese Consonant Sign..Sundanese Vowel Sign Pan
          ( 0x1BA8, 0x1BA9 )
        , -- Sundanese Vowel Sign Pam..Sundanese Vowel Sign Pan
          ( 0x1BAB, 0x1BAD )
        , -- Sundanese Sign Virama   ..Sundanese Consonant Sign
          ( 0x1BE6, 0x1BE6 )
        , -- Batak Sign Tompi        ..Batak Sign Tompi
          ( 0x1BE8, 0x1BE9 )
        , -- Batak Vowel Sign Pakpak ..Batak Vowel Sign Ee
          ( 0x1BED, 0x1BED )
        , -- Batak Vowel Sign Karo O ..Batak Vowel Sign Karo O
          ( 0x1BEF, 0x1BF1 )
        , -- Batak Vowel Sign U For S..Batak Consonant Sign H
          ( 0x1C2C, 0x1C33 )
        , -- Lepcha Vowel Sign E     ..Lepcha Consonant Sign T
          ( 0x1C36, 0x1C37 )
        , -- Lepcha Sign Ran         ..Lepcha Sign Nukta
          ( 0x1CD0, 0x1CD2 )
        , -- Vedic Tone Karshana     ..Vedic Tone Prenkha
          ( 0x1CD4, 0x1CE0 )
        , -- Vedic Sign Yajurvedic Mi..Vedic Tone Rigvedic Kash
          ( 0x1CE2, 0x1CE8 )
        , -- Vedic Sign Visarga Svari..Vedic Sign Visarga Anuda
          ( 0x1CED, 0x1CED )
        , -- Vedic Sign Tiryak       ..Vedic Sign Tiryak
          ( 0x1CF4, 0x1CF4 )
        , -- Vedic Tone Candra Above ..Vedic Tone Candra Above
          ( 0x1CF8, 0x1CF9 )
        , -- Vedic Tone Ring Above   ..Vedic Tone Double Ring A
          ( 0x1DC0, 0x1DF9 )
        , -- Combining Dotted Grave A..Combining Wide Inverted
          ( 0x1DFB, 0x1DFF )
        , -- Combining Deletion Mark ..Combining Right Arrowhea
          ( 0x20D0, 0x20F0 )
        , -- Combining Left Harpoon A..Combining Asterisk Above
          ( 0x2CEF, 0x2CF1 )
        , -- Coptic Combining Ni Abov..Coptic Combining Spiritu
          ( 0x2D7F, 0x2D7F )
        , -- Tifinagh Consonant Joine..Tifinagh Consonant Joine
          ( 0x2DE0, 0x2DFF )
        , -- Combining Cyrillic Lette..Combining Cyrillic Lette
          ( 0x302A, 0x302D )
        , -- Ideographic Level Tone M..Ideographic Entering Ton
          ( 0x3099, 0x309A )
        , -- Combining Katakana-hirag..Combining Katakana-hirag
          ( 0xA66F, 0xA672 )
        , -- Combining Cyrillic Vzmet..Combining Cyrillic Thous
          ( 0xA674, 0xA67D )
        , -- Combining Cyrillic Lette..Combining Cyrillic Payer
          ( 0xA69E, 0xA69F )
        , -- Combining Cyrillic Lette..Combining Cyrillic Lette
          ( 0xA6F0, 0xA6F1 )
        , -- Bamum Combining Mark Koq..Bamum Combining Mark Tuk
          ( 0xA802, 0xA802 )
        , -- Syloti Nagri Sign Dvisva..Syloti Nagri Sign Dvisva
          ( 0xA806, 0xA806 )
        , -- Syloti Nagri Sign Hasant..Syloti Nagri Sign Hasant
          ( 0xA80B, 0xA80B )
        , -- Syloti Nagri Sign Anusva..Syloti Nagri Sign Anusva
          ( 0xA825, 0xA826 )
        , -- Syloti Nagri Vowel Sign ..Syloti Nagri Vowel Sign
          ( 0xA82C, 0xA82C )
        , -- (nil)                   ..(nil)
          ( 0xA8C4, 0xA8C5 )
        , -- Saurashtra Sign Virama  ..Saurashtra Sign Candrabi
          ( 0xA8E0, 0xA8F1 )
        , -- Combining Devanagari Dig..Combining Devanagari Sig
          ( 0xA8FF, 0xA8FF )
        , -- Devanagari Vowel Sign Ay..Devanagari Vowel Sign Ay
          ( 0xA926, 0xA92D )
        , -- Kayah Li Vowel Ue       ..Kayah Li Tone Calya Plop
          ( 0xA947, 0xA951 )
        , -- Rejang Vowel Sign I     ..Rejang Consonant Sign R
          ( 0xA980, 0xA982 )
        , -- Javanese Sign Panyangga ..Javanese Sign Layar
          ( 0xA9B3, 0xA9B3 )
        , -- Javanese Sign Cecak Telu..Javanese Sign Cecak Telu
          ( 0xA9B6, 0xA9B9 )
        , -- Javanese Vowel Sign Wulu..Javanese Vowel Sign Suku
          ( 0xA9BC, 0xA9BD )
        , -- Javanese Vowel Sign Pepe..Javanese Consonant Sign
          ( 0xA9E5, 0xA9E5 )
        , -- Myanmar Sign Shan Saw   ..Myanmar Sign Shan Saw
          ( 0xAA29, 0xAA2E )
        , -- Cham Vowel Sign Aa      ..Cham Vowel Sign Oe
          ( 0xAA31, 0xAA32 )
        , -- Cham Vowel Sign Au      ..Cham Vowel Sign Ue
          ( 0xAA35, 0xAA36 )
        , -- Cham Consonant Sign La  ..Cham Consonant Sign Wa
          ( 0xAA43, 0xAA43 )
        , -- Cham Consonant Sign Fina..Cham Consonant Sign Fina
          ( 0xAA4C, 0xAA4C )
        , -- Cham Consonant Sign Fina..Cham Consonant Sign Fina
          ( 0xAA7C, 0xAA7C )
        , -- Myanmar Sign Tai Laing T..Myanmar Sign Tai Laing T
          ( 0xAAB0, 0xAAB0 )
        , -- Tai Viet Mai Kang       ..Tai Viet Mai Kang
          ( 0xAAB2, 0xAAB4 )
        , -- Tai Viet Vowel I        ..Tai Viet Vowel U
          ( 0xAAB7, 0xAAB8 )
        , -- Tai Viet Mai Khit       ..Tai Viet Vowel Ia
          ( 0xAABE, 0xAABF )
        , -- Tai Viet Vowel Am       ..Tai Viet Tone Mai Ek
          ( 0xAAC1, 0xAAC1 )
        , -- Tai Viet Tone Mai Tho   ..Tai Viet Tone Mai Tho
          ( 0xAAEC, 0xAAED )
        , -- Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
          ( 0xAAF6, 0xAAF6 )
        , -- Meetei Mayek Virama     ..Meetei Mayek Virama
          ( 0xABE5, 0xABE5 )
        , -- Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
          ( 0xABE8, 0xABE8 )
        , -- Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
          ( 0xABED, 0xABED )
        , -- Meetei Mayek Apun Iyek  ..Meetei Mayek Apun Iyek
          ( 0xFB1E, 0xFB1E )
        , -- Hebrew Point Judeo-spani..Hebrew Point Judeo-spani
          ( 0xFE00, 0xFE0F )
        , -- Variation Selector-1    ..Variation Selector-16
          ( 0xFE20, 0xFE2F )
        , -- Combining Ligature Left ..Combining Cyrillic Titlo
          ( 0x000101FD, 0x000101FD )
        , -- Phaistos Disc Sign Combi..Phaistos Disc Sign Combi
          ( 0x000102E0, 0x000102E0 )
        , -- Coptic Epact Thousands M..Coptic Epact Thousands M
          ( 0x00010376, 0x0001037A )
        , -- Combining Old Permic Let..Combining Old Permic Let
          ( 0x00010A01, 0x00010A03 )
        , -- Kharoshthi Vowel Sign I ..Kharoshthi Vowel Sign Vo
          ( 0x00010A05, 0x00010A06 )
        , -- Kharoshthi Vowel Sign E ..Kharoshthi Vowel Sign O
          ( 0x00010A0C, 0x00010A0F )
        , -- Kharoshthi Vowel Length ..Kharoshthi Sign Visarga
          ( 0x00010A38, 0x00010A3A )
        , -- Kharoshthi Sign Bar Abov..Kharoshthi Sign Dot Belo
          ( 0x00010A3F, 0x00010A3F )
        , -- Kharoshthi Virama       ..Kharoshthi Virama
          ( 0x00010AE5, 0x00010AE6 )
        , -- Manichaean Abbreviation ..Manichaean Abbreviation
          ( 0x00010D24, 0x00010D27 )
        , -- Hanifi Rohingya Sign Har..Hanifi Rohingya Sign Tas
          ( 0x00010EAB, 0x00010EAC )
        , -- (nil)                   ..(nil)
          ( 0x00010F46, 0x00010F50 )
        , -- Sogdian Combining Dot Be..Sogdian Combining Stroke
          ( 0x00011001, 0x00011001 )
        , -- Brahmi Sign Anusvara    ..Brahmi Sign Anusvara
          ( 0x00011038, 0x00011046 )
        , -- Brahmi Vowel Sign Aa    ..Brahmi Virama
          ( 0x0001107F, 0x00011081 )
        , -- Brahmi Number Joiner    ..Kaithi Sign Anusvara
          ( 0x000110B3, 0x000110B6 )
        , -- Kaithi Vowel Sign U     ..Kaithi Vowel Sign Ai
          ( 0x000110B9, 0x000110BA )
        , -- Kaithi Sign Virama      ..Kaithi Sign Nukta
          ( 0x00011100, 0x00011102 )
        , -- Chakma Sign Candrabindu ..Chakma Sign Visarga
          ( 0x00011127, 0x0001112B )
        , -- Chakma Vowel Sign A     ..Chakma Vowel Sign Uu
          ( 0x0001112D, 0x00011134 )
        , -- Chakma Vowel Sign Ai    ..Chakma Maayyaa
          ( 0x00011173, 0x00011173 )
        , -- Mahajani Sign Nukta     ..Mahajani Sign Nukta
          ( 0x00011180, 0x00011181 )
        , -- Sharada Sign Candrabindu..Sharada Sign Anusvara
          ( 0x000111B6, 0x000111BE )
        , -- Sharada Vowel Sign U    ..Sharada Vowel Sign O
          ( 0x000111C9, 0x000111CC )
        , -- Sharada Sandhi Mark     ..Sharada Extra Short Vowe
          ( 0x000111CF, 0x000111CF )
        , -- (nil)                   ..(nil)
          ( 0x0001122F, 0x00011231 )
        , -- Khojki Vowel Sign U     ..Khojki Vowel Sign Ai
          ( 0x00011234, 0x00011234 )
        , -- Khojki Sign Anusvara    ..Khojki Sign Anusvara
          ( 0x00011236, 0x00011237 )
        , -- Khojki Sign Nukta       ..Khojki Sign Shadda
          ( 0x0001123E, 0x0001123E )
        , -- Khojki Sign Sukun       ..Khojki Sign Sukun
          ( 0x000112DF, 0x000112DF )
        , -- Khudawadi Sign Anusvara ..Khudawadi Sign Anusvara
          ( 0x000112E3, 0x000112EA )
        , -- Khudawadi Vowel Sign U  ..Khudawadi Sign Virama
          ( 0x00011300, 0x00011301 )
        , -- Grantha Sign Combining A..Grantha Sign Candrabindu
          ( 0x0001133B, 0x0001133C )
        , -- Combining Bindu Below   ..Grantha Sign Nukta
          ( 0x00011340, 0x00011340 )
        , -- Grantha Vowel Sign Ii   ..Grantha Vowel Sign Ii
          ( 0x00011366, 0x0001136C )
        , -- Combining Grantha Digit ..Combining Grantha Digit
          ( 0x00011370, 0x00011374 )
        , -- Combining Grantha Letter..Combining Grantha Letter
          ( 0x00011438, 0x0001143F )
        , -- Newa Vowel Sign U       ..Newa Vowel Sign Ai
          ( 0x00011442, 0x00011444 )
        , -- Newa Sign Virama        ..Newa Sign Anusvara
          ( 0x00011446, 0x00011446 )
        , -- Newa Sign Nukta         ..Newa Sign Nukta
          ( 0x0001145E, 0x0001145E )
        , -- Newa Sandhi Mark        ..Newa Sandhi Mark
          ( 0x000114B3, 0x000114B8 )
        , -- Tirhuta Vowel Sign U    ..Tirhuta Vowel Sign Vocal
          ( 0x000114BA, 0x000114BA )
        , -- Tirhuta Vowel Sign Short..Tirhuta Vowel Sign Short
          ( 0x000114BF, 0x000114C0 )
        , -- Tirhuta Sign Candrabindu..Tirhuta Sign Anusvara
          ( 0x000114C2, 0x000114C3 )
        , -- Tirhuta Sign Virama     ..Tirhuta Sign Nukta
          ( 0x000115B2, 0x000115B5 )
        , -- Siddham Vowel Sign U    ..Siddham Vowel Sign Vocal
          ( 0x000115BC, 0x000115BD )
        , -- Siddham Sign Candrabindu..Siddham Sign Anusvara
          ( 0x000115BF, 0x000115C0 )
        , -- Siddham Sign Virama     ..Siddham Sign Nukta
          ( 0x000115DC, 0x000115DD )
        , -- Siddham Vowel Sign Alter..Siddham Vowel Sign Alter
          ( 0x00011633, 0x0001163A )
        , -- Modi Vowel Sign U       ..Modi Vowel Sign Ai
          ( 0x0001163D, 0x0001163D )
        , -- Modi Sign Anusvara      ..Modi Sign Anusvara
          ( 0x0001163F, 0x00011640 )
        , -- Modi Sign Virama        ..Modi Sign Ardhacandra
          ( 0x000116AB, 0x000116AB )
        , -- Takri Sign Anusvara     ..Takri Sign Anusvara
          ( 0x000116AD, 0x000116AD )
        , -- Takri Vowel Sign Aa     ..Takri Vowel Sign Aa
          ( 0x000116B0, 0x000116B5 )
        , -- Takri Vowel Sign U      ..Takri Vowel Sign Au
          ( 0x000116B7, 0x000116B7 )
        , -- Takri Sign Nukta        ..Takri Sign Nukta
          ( 0x0001171D, 0x0001171F )
        , -- Ahom Consonant Sign Medi..Ahom Consonant Sign Medi
          ( 0x00011722, 0x00011725 )
        , -- Ahom Vowel Sign I       ..Ahom Vowel Sign Uu
          ( 0x00011727, 0x0001172B )
        , -- Ahom Vowel Sign Aw      ..Ahom Sign Killer
          ( 0x0001182F, 0x00011837 )
        , -- Dogra Vowel Sign U      ..Dogra Sign Anusvara
          ( 0x00011839, 0x0001183A )
        , -- Dogra Sign Virama       ..Dogra Sign Nukta
          ( 0x0001193B, 0x0001193C )
        , -- (nil)                   ..(nil)
          ( 0x0001193E, 0x0001193E )
        , -- (nil)                   ..(nil)
          ( 0x00011943, 0x00011943 )
        , -- (nil)                   ..(nil)
          ( 0x000119D4, 0x000119D7 )
        , -- Nandinagari Vowel Sign U..Nandinagari Vowel Sign V
          ( 0x000119DA, 0x000119DB )
        , -- Nandinagari Vowel Sign E..Nandinagari Vowel Sign A
          ( 0x000119E0, 0x000119E0 )
        , -- Nandinagari Sign Virama ..Nandinagari Sign Virama
          ( 0x00011A01, 0x00011A0A )
        , -- Zanabazar Square Vowel S..Zanabazar Square Vowel L
          ( 0x00011A33, 0x00011A38 )
        , -- Zanabazar Square Final C..Zanabazar Square Sign An
          ( 0x00011A3B, 0x00011A3E )
        , -- Zanabazar Square Cluster..Zanabazar Square Cluster
          ( 0x00011A47, 0x00011A47 )
        , -- Zanabazar Square Subjoin..Zanabazar Square Subjoin
          ( 0x00011A51, 0x00011A56 )
        , -- Soyombo Vowel Sign I    ..Soyombo Vowel Sign Oe
          ( 0x00011A59, 0x00011A5B )
        , -- Soyombo Vowel Sign Vocal..Soyombo Vowel Length Mar
          ( 0x00011A8A, 0x00011A96 )
        , -- Soyombo Final Consonant ..Soyombo Sign Anusvara
          ( 0x00011A98, 0x00011A99 )
        , -- Soyombo Gemination Mark ..Soyombo Subjoiner
          ( 0x00011C30, 0x00011C36 )
        , -- Bhaiksuki Vowel Sign I  ..Bhaiksuki Vowel Sign Voc
          ( 0x00011C38, 0x00011C3D )
        , -- Bhaiksuki Vowel Sign E  ..Bhaiksuki Sign Anusvara
          ( 0x00011C3F, 0x00011C3F )
        , -- Bhaiksuki Sign Virama   ..Bhaiksuki Sign Virama
          ( 0x00011C92, 0x00011CA7 )
        , -- Marchen Subjoined Letter..Marchen Subjoined Letter
          ( 0x00011CAA, 0x00011CB0 )
        , -- Marchen Subjoined Letter..Marchen Vowel Sign Aa
          ( 0x00011CB2, 0x00011CB3 )
        , -- Marchen Vowel Sign U    ..Marchen Vowel Sign E
          ( 0x00011CB5, 0x00011CB6 )
        , -- Marchen Sign Anusvara   ..Marchen Sign Candrabindu
          ( 0x00011D31, 0x00011D36 )
        , -- Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
          ( 0x00011D3A, 0x00011D3A )
        , -- Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
          ( 0x00011D3C, 0x00011D3D )
        , -- Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
          ( 0x00011D3F, 0x00011D45 )
        , -- Masaram Gondi Vowel Sign..Masaram Gondi Virama
          ( 0x00011D47, 0x00011D47 )
        , -- Masaram Gondi Ra-kara   ..Masaram Gondi Ra-kara
          ( 0x00011D90, 0x00011D91 )
        , -- Gunjala Gondi Vowel Sign..Gunjala Gondi Vowel Sign
          ( 0x00011D95, 0x00011D95 )
        , -- Gunjala Gondi Sign Anusv..Gunjala Gondi Sign Anusv
          ( 0x00011D97, 0x00011D97 )
        , -- Gunjala Gondi Virama    ..Gunjala Gondi Virama
          ( 0x00011EF3, 0x00011EF4 )
        , -- Makasar Vowel Sign I    ..Makasar Vowel Sign U
          ( 0x00016AF0, 0x00016AF4 )
        , -- Bassa Vah Combining High..Bassa Vah Combining High
          ( 0x00016B30, 0x00016B36 )
        , -- Pahawh Hmong Mark Cim Tu..Pahawh Hmong Mark Cim Ta
          ( 0x00016F4F, 0x00016F4F )
        , -- Miao Sign Consonant Modi..Miao Sign Consonant Modi
          ( 0x00016F8F, 0x00016F92 )
        , -- Miao Tone Right         ..Miao Tone Below
          ( 0x00016FE4, 0x00016FE4 )
        , -- (nil)                   ..(nil)
          ( 0x0001BC9D, 0x0001BC9E )
        , -- Duployan Thick Letter Se..Duployan Double Mark
          ( 0x0001D167, 0x0001D169 )
        , -- Musical Symbol Combining..Musical Symbol Combining
          ( 0x0001D17B, 0x0001D182 )
        , -- Musical Symbol Combining..Musical Symbol Combining
          ( 0x0001D185, 0x0001D18B )
        , -- Musical Symbol Combining..Musical Symbol Combining
          ( 0x0001D1AA, 0x0001D1AD )
        , -- Musical Symbol Combining..Musical Symbol Combining
          ( 0x0001D242, 0x0001D244 )
        , -- Combining Greek Musical ..Combining Greek Musical
          ( 0x0001DA00, 0x0001DA36 )
        , -- Signwriting Head Rim    ..Signwriting Air Sucking
          ( 0x0001DA3B, 0x0001DA6C )
        , -- Signwriting Mouth Closed..Signwriting Excitement
          ( 0x0001DA75, 0x0001DA75 )
        , -- Signwriting Upper Body T..Signwriting Upper Body T
          ( 0x0001DA84, 0x0001DA84 )
        , -- Signwriting Location Hea..Signwriting Location Hea
          ( 0x0001DA9B, 0x0001DA9F )
        , -- Signwriting Fill Modifie..Signwriting Fill Modifie
          ( 0x0001DAA1, 0x0001DAAF )
        , -- Signwriting Rotation Mod..Signwriting Rotation Mod
          ( 0x0001E000, 0x0001E006 )
        , -- Combining Glagolitic Let..Combining Glagolitic Let
          ( 0x0001E008, 0x0001E018 )
        , -- Combining Glagolitic Let..Combining Glagolitic Let
          ( 0x0001E01B, 0x0001E021 )
        , -- Combining Glagolitic Let..Combining Glagolitic Let
          ( 0x0001E023, 0x0001E024 )
        , -- Combining Glagolitic Let..Combining Glagolitic Let
          ( 0x0001E026, 0x0001E02A )
        , -- Combining Glagolitic Let..Combining Glagolitic Let
          ( 0x0001E130, 0x0001E136 )
        , -- Nyiakeng Puachue Hmong T..Nyiakeng Puachue Hmong T
          ( 0x0001E2EC, 0x0001E2EF )
        , -- Wancho Tone Tup         ..Wancho Tone Koini
          ( 0x0001E8D0, 0x0001E8D6 )
        , -- Mende Kikakui Combining ..Mende Kikakui Combining
          ( 0x0001E944, 0x0001E94A )
        , -- Adlam Alif Lengthener   ..Adlam Nukta
          ( 0x000E0100, 0x000E01EF ) -- Variation Selector-17   ..Variation Selector-256
        ]


wideEastern : CharTable
wideEastern =
    Array.fromList
        [ ( 0x1100, 0x115F )
        , -- Hangul Choseong Kiyeok  ..Hangul Choseong Filler
          ( 0x231A, 0x231B )
        , -- Watch                   ..Hourglass
          ( 0x2329, 0x232A )
        , -- Left-pointing Angle Brac..Right-pointing Angle Bra
          ( 0x23E9, 0x23EC )
        , -- Black Right-pointing Dou..Black Down-pointing Doub
          ( 0x23F0, 0x23F0 )
        , -- Alarm Clock             ..Alarm Clock
          ( 0x23F3, 0x23F3 )
        , -- Hourglass With Flowing S..Hourglass With Flowing S
          ( 0x25FD, 0x25FE )
        , -- White Medium Small Squar..Black Medium Small Squar
          ( 0x2614, 0x2615 )
        , -- Umbrella With Rain Drops..Hot Beverage
          ( 0x2648, 0x2653 )
        , -- Aries                   ..Pisces
          ( 0x267F, 0x267F )
        , -- Wheelchair Symbol       ..Wheelchair Symbol
          ( 0x2693, 0x2693 )
        , -- Anchor                  ..Anchor
          ( 0x26A1, 0x26A1 )
        , -- High Voltage Sign       ..High Voltage Sign
          ( 0x26AA, 0x26AB )
        , -- Medium White Circle     ..Medium Black Circle
          ( 0x26BD, 0x26BE )
        , -- Soccer Ball             ..Baseball
          ( 0x26C4, 0x26C5 )
        , -- Snowman Without Snow    ..Sun Behind Cloud
          ( 0x26CE, 0x26CE )
        , -- Ophiuchus               ..Ophiuchus
          ( 0x26D4, 0x26D4 )
        , -- No Entry                ..No Entry
          ( 0x26EA, 0x26EA )
        , -- Church                  ..Church
          ( 0x26F2, 0x26F3 )
        , -- Fountain                ..Flag In Hole
          ( 0x26F5, 0x26F5 )
        , -- Sailboat                ..Sailboat
          ( 0x26FA, 0x26FA )
        , -- Tent                    ..Tent
          ( 0x26FD, 0x26FD )
        , -- Fuel Pump               ..Fuel Pump
          ( 0x2705, 0x2705 )
        , -- White Heavy Check Mark  ..White Heavy Check Mark
          ( 0x270A, 0x270B )
        , -- Raised Fist             ..Raised Hand
          ( 0x2728, 0x2728 )
        , -- Sparkles                ..Sparkles
          ( 0x274C, 0x274C )
        , -- Cross Mark              ..Cross Mark
          ( 0x274E, 0x274E )
        , -- Negative Squared Cross M..Negative Squared Cross M
          ( 0x2753, 0x2755 )
        , -- Black Question Mark Orna..White Exclamation Mark O
          ( 0x2757, 0x2757 )
        , -- Heavy Exclamation Mark S..Heavy Exclamation Mark S
          ( 0x2795, 0x2797 )
        , -- Heavy Plus Sign         ..Heavy Division Sign
          ( 0x27B0, 0x27B0 )
        , -- Curly Loop              ..Curly Loop
          ( 0x27BF, 0x27BF )
        , -- Double Curly Loop       ..Double Curly Loop
          ( 0x2B1B, 0x2B1C )
        , -- Black Large Square      ..White Large Square
          ( 0x2B50, 0x2B50 )
        , -- White Medium Star       ..White Medium Star
          ( 0x2B55, 0x2B55 )
        , -- Heavy Large Circle      ..Heavy Large Circle
          ( 0x2E80, 0x2E99 )
        , -- Cjk Radical Repeat      ..Cjk Radical Rap
          ( 0x2E9B, 0x2EF3 )
        , -- Cjk Radical Choke       ..Cjk Radical C-simplified
          ( 0x2F00, 0x2FD5 )
        , -- Kangxi Radical One      ..Kangxi Radical Flute
          ( 0x2FF0, 0x2FFB )
        , -- Ideographic Description ..Ideographic Description
          ( 0x3000, 0x303E )
        , -- Ideographic Space       ..Ideographic Variation In
          ( 0x3041, 0x3096 )
        , -- Hiragana Letter Small A ..Hiragana Letter Small Ke
          ( 0x3099, 0x30FF )
        , -- Combining Katakana-hirag..Katakana Digraph Koto
          ( 0x3105, 0x312F )
        , -- Bopomofo Letter B       ..Bopomofo Letter Nn
          ( 0x3131, 0x318E )
        , -- Hangul Letter Kiyeok    ..Hangul Letter Araeae
          ( 0x3190, 0x31E3 )
        , -- Ideographic Annotation L..Cjk Stroke Q
          ( 0x31F0, 0x321E )
        , -- Katakana Letter Small Ku..Parenthesized Korean Cha
          ( 0x3220, 0x3247 )
        , -- Parenthesized Ideograph ..Circled Ideograph Koto
          ( 0x3250, 0x4DBF )
        , -- Partnership Sign        ..(nil)
          ( 0x4E00, 0xA48C )
        , -- Cjk Unified Ideograph-4e..Yi Syllable Yyr
          ( 0xA490, 0xA4C6 )
        , -- Yi Radical Qot          ..Yi Radical Ke
          ( 0xA960, 0xA97C )
        , -- Hangul Choseong Tikeut-m..Hangul Choseong Ssangyeo
          ( 0xAC00, 0xD7A3 )
        , -- Hangul Syllable Ga      ..Hangul Syllable Hih
          ( 0xF900, 0xFAFF )
        , -- Cjk Compatibility Ideogr..(nil)
          ( 0xFE10, 0xFE19 )
        , -- Presentation Form For Ve..Presentation Form For Ve
          ( 0xFE30, 0xFE52 )
        , -- Presentation Form For Ve..Small Full Stop
          ( 0xFE54, 0xFE66 )
        , -- Small Semicolon         ..Small Equals Sign
          ( 0xFE68, 0xFE6B )
        , -- Small Reverse Solidus   ..Small Commercial At
          ( 0xFF01, 0xFF60 )
        , -- Fullwidth Exclamation Ma..Fullwidth Right White Pa
          ( 0xFFE0, 0xFFE6 )
        , -- Fullwidth Cent Sign     ..Fullwidth Won Sign
          ( 0x00016FE0, 0x00016FE4 )
        , -- Tangut Iteration Mark   ..(nil)
          ( 0x00016FF0, 0x00016FF1 )
        , -- (nil)                   ..(nil)
          ( 0x00017000, 0x000187F7 )
        , -- (nil)                   ..(nil)
          ( 0x00018800, 0x00018CD5 )
        , -- Tangut Component-001    ..(nil)
          ( 0x00018D00, 0x00018D08 )
        , -- (nil)                   ..(nil)
          ( 0x0001B000, 0x0001B11E )
        , -- Katakana Letter Archaic ..Hentaigana Letter N-mu-m
          ( 0x0001B150, 0x0001B152 )
        , -- Hiragana Letter Small Wi..Hiragana Letter Small Wo
          ( 0x0001B164, 0x0001B167 )
        , -- Katakana Letter Small Wi..Katakana Letter Small N
          ( 0x0001B170, 0x0001B2FB )
        , -- Nushu Character-1b170   ..Nushu Character-1b2fb
          ( 0x0001F004, 0x0001F004 )
        , -- Mahjong Tile Red Dragon ..Mahjong Tile Red Dragon
          ( 0x0001F0CF, 0x0001F0CF )
        , -- Playing Card Black Joker..Playing Card Black Joker
          ( 0x0001F18E, 0x0001F18E )
        , -- Negative Squared Ab     ..Negative Squared Ab
          ( 0x0001F191, 0x0001F19A )
        , -- Squared Cl              ..Squared Vs
          ( 0x0001F200, 0x0001F202 )
        , -- Square Hiragana Hoka    ..Squared Katakana Sa
          ( 0x0001F210, 0x0001F23B )
        , -- Squared Cjk Unified Ideo..Squared Cjk Unified Ideo
          ( 0x0001F240, 0x0001F248 )
        , -- Tortoise Shell Bracketed..Tortoise Shell Bracketed
          ( 0x0001F250, 0x0001F251 )
        , -- Circled Ideograph Advant..Circled Ideograph Accept
          ( 0x0001F260, 0x0001F265 )
        , -- Rounded Symbol For Fu   ..Rounded Symbol For Cai
          ( 0x0001F300, 0x0001F320 )
        , -- Cyclone                 ..Shooting Star
          ( 0x0001F32D, 0x0001F335 )
        , -- Hot Dog                 ..Cactus
          ( 0x0001F337, 0x0001F37C )
        , -- Tulip                   ..Baby Bottle
          ( 0x0001F37E, 0x0001F393 )
        , -- Bottle With Popping Cork..Graduation Cap
          ( 0x0001F3A0, 0x0001F3CA )
        , -- Carousel Horse          ..Swimmer
          ( 0x0001F3CF, 0x0001F3D3 )
        , -- Cricket Bat And Ball    ..Table Tennis Paddle And
          ( 0x0001F3E0, 0x0001F3F0 )
        , -- House Building          ..European Castle
          ( 0x0001F3F4, 0x0001F3F4 )
        , -- Waving Black Flag       ..Waving Black Flag
          ( 0x0001F3F8, 0x0001F43E )
        , -- Badminton Racquet And Sh..Paw Prints
          ( 0x0001F440, 0x0001F440 )
        , -- Eyes                    ..Eyes
          ( 0x0001F442, 0x0001F4FC )
        , -- Ear                     ..Videocassette
          ( 0x0001F4FF, 0x0001F53D )
        , -- Prayer Beads            ..Down-pointing Small Red
          ( 0x0001F54B, 0x0001F54E )
        , -- Kaaba                   ..Menorah With Nine Branch
          ( 0x0001F550, 0x0001F567 )
        , -- Clock Face One Oclock   ..Clock Face Twelve-thirty
          ( 0x0001F57A, 0x0001F57A )
        , -- Man Dancing             ..Man Dancing
          ( 0x0001F595, 0x0001F596 )
        , -- Reversed Hand With Middl..Raised Hand With Part Be
          ( 0x0001F5A4, 0x0001F5A4 )
        , -- Black Heart             ..Black Heart
          ( 0x0001F5FB, 0x0001F64F )
        , -- Mount Fuji              ..Person With Folded Hands
          ( 0x0001F680, 0x0001F6C5 )
        , -- Rocket                  ..Left Luggage
          ( 0x0001F6CC, 0x0001F6CC )
        , -- Sleeping Accommodation  ..Sleeping Accommodation
          ( 0x0001F6D0, 0x0001F6D2 )
        , -- Place Of Worship        ..Shopping Trolley
          ( 0x0001F6D5, 0x0001F6D7 )
        , -- Hindu Temple            ..(nil)
          ( 0x0001F6EB, 0x0001F6EC )
        , -- Airplane Departure      ..Airplane Arriving
          ( 0x0001F6F4, 0x0001F6FC )
        , -- Scooter                 ..(nil)
          ( 0x0001F7E0, 0x0001F7EB )
        , -- Large Orange Circle     ..Large Brown Square
          ( 0x0001F90C, 0x0001F93A )
        , -- (nil)                   ..Fencer
          ( 0x0001F93C, 0x0001F945 )
        , -- Wrestlers               ..Goal Net
          ( 0x0001F947, 0x0001F978 )
        , -- First Place Medal       ..(nil)
          ( 0x0001F97A, 0x0001F9CB )
        , -- Face With Pleading Eyes ..(nil)
          ( 0x0001F9CD, 0x0001F9FF )
        , -- Standing Person         ..Nazar Amulet
          ( 0x0001FA70, 0x0001FA74 )
        , -- Ballet Shoes            ..(nil)
          ( 0x0001FA78, 0x0001FA7A )
        , -- Drop Of Blood           ..Stethoscope
          ( 0x0001FA80, 0x0001FA86 )
        , -- Yo-yo                   ..(nil)
          ( 0x0001FA90, 0x0001FAA8 )
        , -- Ringed Planet           ..(nil)
          ( 0x0001FAB0, 0x0001FAB6 )
        , -- (nil)                   ..(nil)
          ( 0x0001FAC0, 0x0001FAC2 )
        , -- (nil)                   ..(nil)
          ( 0x0001FAD0, 0x0001FAD6 )
        , -- (nil)                   ..(nil)
          ( 0x00020000, 0x0002FFFD )
        , -- Cjk Unified Ideograph-20..(nil)
          ( 0x00030000, 0x0003FFFD ) -- (nil)                   ..(nil)
        ]
