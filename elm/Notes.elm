module Notes
    exposing
        ( Note
        , Octave
        , OctaveRange
        , Mode
        , scaleMode
        , chordMode
        , chord
        , scale
        , midiNoteNumberToNoteAndOctave
        , midiNoteNumberToString
        , noteAndOctaveToMidiNoteNumber
        , noteNames
        , formulaNames
        )

import WesternMusicData exposing (..)
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



-- removeAdjacentDuplicates and expandCollection should not live in the notes module


removeAdjacentDuplicates : List a -> List a
removeAdjacentDuplicates ls =
    case ls of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: xs ->
            if x == y then
                x :: (removeAdjacentDuplicates xs)
            else
                x :: (removeAdjacentDuplicates (y :: xs))


expandCollection : Mode -> Int -> List Int -> List Int
expandCollection mode times l =
    -- mode is either scaleMode or chordMode
    -- what should we do in chordMode ???
    let
        r =
            List.range 1 times
    in
        r
            |> List.map
                (\i ->
                    l
                        |> List.map (\x -> x + (12 * (i - 1)))
                )
            |> List.concat


noteCollection : Mode -> List Formula -> Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
noteCollection mode collectionType root octave formulaName octaveRange dedup =
    let
        mnn =
            Maybe.withDefault 0 (noteAndOctaveToMidiNoteNumber root octave)

        fml =
            Maybe.withDefault [] (formula collectionType formulaName)
    in
        case dedup of
            False ->
                fml
                    |> List.map (\x -> x + mnn)
                    |> expandCollection mode octaveRange

            True ->
                fml
                    |> List.map (\x -> x + mnn)
                    |> expandCollection mode octaveRange
                    |> removeAdjacentDuplicates


chordCollection : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
chordCollection =
    noteCollection chordMode chordFormulaPool


scaleCollection : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
scaleCollection =
    noteCollection scaleMode scaleFormulaPool


chord : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> Int -> NoteCollection
chord root octave chordName octaveRange dedup inversion =
    chordCollection root octave chordName octaveRange dedup
        |> rotateR inversion


scale : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
scale root octave scaleName octaveRange dedup =
    scaleCollection root octave scaleName octaveRange dedup
