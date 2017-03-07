module Notes exposing (..)

import Array
    exposing
        ( Array
        , fromList
        , get
        )


type alias Mode =
    String


type alias Note =
    String


type alias Octave =
    Int


type alias OctaveRange =
    Int


type alias NoteCollection =
    List Int


type alias FormulaName =
    String


{-| A Formula is a named sequence of intervals that is used to build note collections
    : Chords and scales
-}
type alias Formula =
    ( FormulaName, NoteCollection )


noteNames : List Note
noteNames =
    [ "C"
    , "C#"
    , "D"
    , "D#"
    , "E"
    , "F"
    , "F#"
    , "G"
    , "G#"
    , "A"
    , "A#"
    , "B"
    ]


noteArray : Array Note
noteArray =
    Array.fromList noteNames


scaleMode : Mode
scaleMode =
    "Scale"


chordMode : Mode
chordMode =
    "Chord"


{-|

Looks for a Tuple in the List "list" whose first value is "name" and if found,
returns the corresponding second value"

    formula chordFormulaPool major
-}
formula : List Formula -> FormulaName -> Maybe (List Int)
formula list name =
    case list of
        [] ->
            Nothing

        ( x, f ) :: xs ->
            if x == name then
                Just f
            else
                formula xs name


{-|
    All the names of a collection of formulas
    ie: All Chord names or all Scale names
-}
formulaNames : List Formula -> List FormulaName
formulaNames l =
    List.map (\x -> Tuple.first x) l



-- returns the index (position) of an element in a list


indexInList : List a -> a -> Int -> Maybe Int
indexInList list el base =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x == el then
                Just base
            else
                indexInList xs el (base + 1)


{-| extract the first element and put it at the end of the list "times" times
 Note that the list parameter is second (last) parameter  so we can use the pipe operator

    rotateR 3 [0,1 2 3,4,5] == [3,4,5,0,1,2]
-}
rotateR : Int -> List a -> List a
rotateR times list =
    case times of
        0 ->
            list

        _ ->
            case list of
                [] ->
                    []

                x :: xs ->
                    rotateR (times - 1) (List.append xs [ x ])



{-
   Returns the midi note number in the range of 0..127
   C-1 to G9 or Nothing
-}


noteAndOctaveToMidiNoteNumber : Note -> Octave -> Maybe Int
noteAndOctaveToMidiNoteNumber note octave =
    case indexInList noteNames (String.toUpper note) 0 of
        Nothing ->
            Nothing

        Just nn ->
            let
                noteNumber =
                    (nn + (12 * (octave + 1)))
            in
                if noteNumber < 0 || noteNumber > 127 then
                    Nothing
                else
                    Just noteNumber


midiNoteNumberToString : Int -> String
midiNoteNumberToString noteNumber =
    let
        ( n, o ) =
            midiNoteNumberToNoteAndOctave noteNumber
    in
        n ++ toString o


midiNoteNumberToNoteAndOctave : Int -> ( Note, Octave )
midiNoteNumberToNoteAndOctave noteNumber =
    let
        o =
            noteNumber // 12

        n =
            noteNumber % 12
    in
        ( Maybe.withDefault " " (get n noteArray), o - 1 )


expandCollection : Mode -> Int -> List Int -> List Int
expandCollection mode times l =
    let
        r =
            List.range 1 times

        h =
            Maybe.withDefault 0 (List.head l)

        l1 =
            [ h + (12 * times) ]

        l2 =
            r
                |> List.map
                    (\i ->
                        l
                            |> List.map (\x -> x + (12 * (i - 1)))
                    )
                |> List.concat
    in
        case mode of
            "Scale" ->
                List.append l2 l1

            "Chord" ->
                l2

            _ ->
                []


noteCollection : Mode -> List Formula -> Note -> Octave -> FormulaName -> OctaveRange -> NoteCollection
noteCollection mode collectionType root octave formulaName octaveRange =
    let
        mnn =
            Maybe.withDefault 0 (noteAndOctaveToMidiNoteNumber root octave)

        fml =
            Maybe.withDefault [] (formula collectionType formulaName)
    in
        fml
            |> List.map (\x -> x + mnn)
            |> expandCollection mode octaveRange


chordCollection : Note -> Octave -> FormulaName -> OctaveRange -> NoteCollection
chordCollection =
    noteCollection chordMode chordFormulaPool


scaleCollection : Note -> Octave -> FormulaName -> OctaveRange -> NoteCollection
scaleCollection =
    noteCollection scaleMode scaleFormulaPool


chord : Note -> Octave -> FormulaName -> OctaveRange -> Int -> NoteCollection
chord root octave chordName octaveRange inversion =
    chordCollection root octave chordName octaveRange
        |> rotateR inversion


scale : Note -> Octave -> FormulaName -> OctaveRange -> NoteCollection
scale root octave scaleName octaveRange =
    scaleCollection root octave scaleName octaveRange



--DATA--


chordFormulaPool : List Formula
chordFormulaPool =
    [ ( major, [ 0, 4, 7 ] )
    , ( major6, [ 0, 4, 7, 9 ] )
    , ( major7, [ 0, 4, 7, 11 ] )
    , ( major9, [ 0, 4, 7, 11, 14 ] )
    , ( major69, [ 0, 4, 7, 9, 14 ] )
    , ( major11, [ 0, 4, 7, 11, 14, 17 ] )
    , ( major13, [ 0, 4, 7, 11, 14, 17, 21 ] )
    , ( minor, [ 0, 3, 7 ] )
    , ( minor6, [ 0, 3, 7, 9 ] )
    , ( minor7, [ 0, 3, 7, 10 ] )
    , ( minor9, [ 0, 3, 7, 10, 14 ] )
    , ( minor69, [ 0, 3, 7, 9, 14 ] )
    , ( minor11, [ 0, 3, 7, 10, 14, 17 ] )
    , ( minor13, [ 0, 3, 7, 10, 14, 17, 21 ] )
    , ( dominant7, [ 0, 4, 7, 10 ] )
    , ( ninth, [ 0, 4, 7, 10, 14 ] )
    , ( eleventh, [ 0, 4, 7, 10, 14, 17 ] )
    , ( thirteenth, [ 0, 4, 7, 10, 14, 17, 21 ] )
    , ( diminished, [ 0, 3, 6 ] )
    , ( halfDiminished7, [ 0, 3, 6, 10 ] )
    , ( diminished7, [ 0, 3, 6, 9 ] )
    , ( augmented, [ 0, 4, 8 ] )
    , ( augmented7, [ 0, 4, 8, 10 ] )
    , ( sus4, [ 0, 5, 7 ] )
    , ( seventhSus4, [ 0, 5, 7, 10 ] )
    , ( minorMajor, [ 0, 3, 7, 11 ] )
    ]


scaleFormulaPool : List Formula
scaleFormulaPool =
    [ ( chromatic, [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] )
    , ( ionian, [ 0, 2, 4, 5, 7, 9, 11, 12 ] )
    , ( dorian, [ 0, 2, 3, 5, 7, 9, 10, 12 ] )
    , ( phrygian, [ 0, 1, 3, 5, 7, 8, 10, 12 ] )
    , ( lydian, [ 0, 2, 4, 6, 7, 9, 11, 12 ] )
    , ( mixolydian, [ 0, 2, 4, 5, 7, 9, 10, 12 ] )
    , ( aeolian, [ 0, 2, 3, 5, 7, 8, 10, 12 ] )
    , ( locrian, [ 0, 1, 3, 5, 6, 8, 10, 12 ] )
    , ( naturalMinor, [ 0, 2, 3, 5, 7, 8, 10, 12 ] )
    , ( harmonicMinor, [ 0, 2, 3, 5, 7, 8, 11, 12 ] )
    , ( melodicMinor, [ 0, 2, 3, 5, 7, 9, 11, 12 ] )
    , ( gypsyMinor, [ 0, 2, 3, 6, 7, 8, 11, 12 ] )
    , ( wholeTone, [ 0, 2, 4, 6, 8, 10, 12 ] )
    , ( majorPentatonic, [ 0, 2, 4, 7, 9, 12 ] )
    , ( minorPentatonic, [ 0, 3, 5, 7, 10, 12 ] )
    , ( majorBebop, [ 0, 2, 4, 5, 7, 8, 9, 11, 12 ] )
    , ( alteredScale, [ 0, 1, 3, 4, 6, 8, 10, 12 ] )
    , ( dorianBebop, [ 0, 2, 3, 4, 5, 7, 9, 10, 12 ] )
    , ( mixolydianBebop, [ 0, 2, 4, 5, 7, 9, 10, 11, 12 ] )
    , ( bluesScale, [ 0, 3, 5, 6, 7, 10, 12 ] )
    , ( diminishedWholeHalf, [ 0, 2, 3, 5, 6, 8, 9, 11, 12 ] )
    , ( diminishedHalfWhole, [ 0, 1, 3, 4, 6, 7, 9, 10, 12 ] )
    , ( neapolitanMajor, [ 0, 1, 3, 5, 7, 9, 11, 12 ] )
    , ( hungarianMajor, [ 0, 3, 4, 6, 7, 9, 10, 12 ] )
    , ( harmonicMajor, [ 0, 2, 4, 5, 7, 8, 11, 12 ] )
    , ( hungarianMinor, [ 0, 2, 3, 6, 7, 8, 11, 12 ] )
    , ( lydianMinor, [ 0, 2, 4, 6, 7, 8, 10, 12 ] )
    , ( neapolitanMinor, [ 0, 1, 3, 5, 7, 8, 11, 12 ] )
    , ( majorLocrian, [ 0, 2, 4, 5, 6, 8, 10, 12 ] )
    , ( leadingWholeTone, [ 0, 2, 4, 6, 8, 10, 11, 12 ] )
    , ( sixToneSymmetrical, [ 0, 1, 4, 5, 8, 9, 11, 12 ] )
    , ( arabian, [ 0, 2, 4, 5, 6, 8, 10, 12 ] )
    , ( balinese, [ 0, 1, 3, 7, 8, 12 ] )
    , ( byzantine, [ 0, 1, 3, 5, 7, 8, 11, 12 ] )
    , ( hungarianGypsy, [ 0, 2, 4, 6, 7, 8, 10, 12 ] )
    , ( persian, [ 0, 1, 4, 5, 6, 8, 11, 12 ] )
    , ( eastIndianPurvi, [ 0, 1, 4, 6, 7, 8, 11, 12 ] )
    , ( oriental, [ 0, 1, 4, 5, 6, 9, 10, 12 ] )
    , ( doubleHarmonic, [ 0, 1, 4, 5, 7, 8, 11, 12 ] )
    , ( enigmatic, [ 0, 1, 4, 6, 8, 10, 11, 12 ] )
    , ( overtone, [ 0, 2, 4, 6, 7, 9, 10, 12 ] )
    , ( eightToneSpanish, [ 0, 1, 3, 4, 5, 6, 8, 10, 12 ] )
    , ( prometheus, [ 0, 2, 4, 6, 9, 10, 12 ] )
    , ( gagakuRittsuSenPou, [ 0, 2, 5, 7, 9, 10, 12 ] )
    , ( gagakuRyoSenPou, [ 0, 2, 4, 7, 9, 12 ] )
    , ( zokugakuYoSenPou, [ 0, 3, 5, 7, 10, 12 ] )
    , ( inSenPou, [ 0, 1, 5, 2, 8, 12 ] )
    , ( okinawa, [ 0, 4, 5, 7, 11, 12 ] )
    ]


major : FormulaName
major =
    "Major"


major6 : FormulaName
major6 =
    "Major 6"


major7 : FormulaName
major7 =
    "Major 7"


major9 : FormulaName
major9 =
    "Major 9"


major69 : FormulaName
major69 =
    "Major 6 9"


major11 : FormulaName
major11 =
    "Major 11"


major13 : FormulaName
major13 =
    "Major 13"


minor : FormulaName
minor =
    "Minor"


minor6 : FormulaName
minor6 =
    "Minor 6"


minor7 : FormulaName
minor7 =
    "Minor 7"


minor9 : FormulaName
minor9 =
    "Minor 9"


minor69 : FormulaName
minor69 =
    "Minor 6 9"


minor11 : FormulaName
minor11 =
    "Minor 11"


minor13 : FormulaName
minor13 =
    "Minor 13"


dominant7 : FormulaName
dominant7 =
    "Dominant 7"


ninth : FormulaName
ninth =
    "Ninth"


eleventh : FormulaName
eleventh =
    "Eleventh"


thirteenth : FormulaName
thirteenth =
    "Thirteenth"


diminished : FormulaName
diminished =
    "Diminished"


halfDiminished7 : FormulaName
halfDiminished7 =
    "Half Diminished 7"


diminished7 : FormulaName
diminished7 =
    "Diminished 7"


augmented : FormulaName
augmented =
    "Augmented"


augmented7 : FormulaName
augmented7 =
    "Augmented 7"


sus4 : FormulaName
sus4 =
    "Sus 4"


seventhSus4 : FormulaName
seventhSus4 =
    "Seventh Sus 4"


minorMajor : FormulaName
minorMajor =
    "Minor Major"


chromatic : FormulaName
chromatic =
    "Chromatic"


ionian : FormulaName
ionian =
    "Ionian/Major"


dorian : FormulaName
dorian =
    "Dorian"


phrygian : FormulaName
phrygian =
    "Phrygian"


lydian : FormulaName
lydian =
    "Lydian"


mixolydian : FormulaName
mixolydian =
    "Mixolydian"


aeolian : FormulaName
aeolian =
    "Aeolian"


locrian : FormulaName
locrian =
    "Locrian"


naturalMinor : FormulaName
naturalMinor =
    "Natural Minor"


harmonicMinor : FormulaName
harmonicMinor =
    "Harmonic Minor"


melodicMinor : FormulaName
melodicMinor =
    "Melodic Minor"


gypsyMinor : FormulaName
gypsyMinor =
    "Gypsy Minor"


wholeTone : FormulaName
wholeTone =
    "Whole Tone"


majorPentatonic : FormulaName
majorPentatonic =
    "Major Pentatonic"


minorPentatonic : FormulaName
minorPentatonic =
    "Minor Pentatonic"


majorBebop : FormulaName
majorBebop =
    "Major Bebop"


alteredScale : FormulaName
alteredScale =
    "Altered Scale"


dorianBebop : FormulaName
dorianBebop =
    "Dorian Bebop"


mixolydianBebop : FormulaName
mixolydianBebop =
    "Mixolydian Bebop"


bluesScale : FormulaName
bluesScale =
    "Blues Scale"


diminishedWholeHalf : FormulaName
diminishedWholeHalf =
    "Diminished Whole Half"


diminishedHalfWhole : FormulaName
diminishedHalfWhole =
    "Diminished Half Whole"


neapolitanMajor : FormulaName
neapolitanMajor =
    "Neapolitan Major"


hungarianMajor : FormulaName
hungarianMajor =
    "Hungarian Major"


harmonicMajor : FormulaName
harmonicMajor =
    "Harmonic Major"


hungarianMinor : FormulaName
hungarianMinor =
    "Hungarian Minor"


lydianMinor : FormulaName
lydianMinor =
    "Lydian Minor"


neapolitanMinor : FormulaName
neapolitanMinor =
    "Neapolitan Minor"


majorLocrian : FormulaName
majorLocrian =
    "Major Locrian"


leadingWholeTone : FormulaName
leadingWholeTone =
    "Leading Whole Tone"


sixToneSymmetrical : FormulaName
sixToneSymmetrical =
    "Six Tone Symmetrical"


arabian : FormulaName
arabian =
    "Arabian"


balinese : FormulaName
balinese =
    "Balinese"


byzantine : FormulaName
byzantine =
    "Byzantine"


hungarianGypsy : FormulaName
hungarianGypsy =
    "Hungarian Gypsy"


persian : FormulaName
persian =
    "Persian"


eastIndianPurvi : FormulaName
eastIndianPurvi =
    "East Indian Purvi"


oriental : FormulaName
oriental =
    "Oriental"


doubleHarmonic : FormulaName
doubleHarmonic =
    "Double Harmonic"


enigmatic : FormulaName
enigmatic =
    "Enigmatic"


overtone : FormulaName
overtone =
    "Overtone"


eightToneSpanish : FormulaName
eightToneSpanish =
    "Eight Tone Spanish"


prometheus : FormulaName
prometheus =
    "Prometheus"


gagakuRittsuSenPou : FormulaName
gagakuRittsuSenPou =
    "Gagaku Rittsu Sen Pou"


gagakuRyoSenPou : FormulaName
gagakuRyoSenPou =
    "Gagaku Ryo Sen Pou"


zokugakuYoSenPou : FormulaName
zokugakuYoSenPou =
    "Zokugaku Yo Sen Pou"


inSenPou : FormulaName
inSenPou =
    "In Sen Pou"


okinawa : FormulaName
okinawa =
    "Okinawa"
