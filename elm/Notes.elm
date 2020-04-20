module Notes exposing
    ( Mode
    , Note
    , Octave
    , OctaveRange
    , chord
    , chordMode
    , formulaNames
    , midiNoteNumberToNoteAndOctave
    , midiNoteNumberToString
    , noteAndOctaveToMidiNoteNumber
    , noteNames
    , scale
    , scaleMode
    , sequence
    )

import Array
    exposing
        ( Array
        , fromList
        , get
        )
import String exposing (fromInt)
import WesternMusicData
    exposing
        ( Formula
        , FormulaName
        , NoteCollection
        , chordFormulaPool
        , scaleFormulaPool
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



-----------------------Helpers----------------------


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
Note that the list parameter is second (last) parameter so we can use the pipe operator

    rotateR 3 [ 0, 1 2 3, 4, 5 ] == [ 3, 4, 5, 0, 1, 2 ]

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


listOfIntFromString : String -> List Int
listOfIntFromString s =
    let
        -- Convert a 'String' in a 'List Maybe Int' through a 'List Char'
        sequenceIntValue =
            List.map (\x -> String.toInt (String.fromChar x)) (String.toList s)
    in
    -- The following filters out the "Nothing" s ( List Maybe Int -> List Int)
    List.filterMap identity sequenceIntValue



-- This will expand a List like this [ 1,3,4,7] to one like this [ 1,0,3,4,0,0,7]
-- Thanks to https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/s/s15.html


repeatElements : Int -> List a -> List a
repeatElements n list =
    case list of
        [] ->
            []

        x :: xs ->
            List.repeat n x ++ repeatElements n xs


expand : List Int -> List Int
expand l =
    case l of
        [] ->
            []

        x1 :: x2 :: xs ->
            let
                -- Problem with patterns like  [1432] cause descending numbers
                distance =
                    x2 - x1 - 1
            in
            if distance == 0 then
                x1 :: expand (x2 :: xs)

            else
                x1 :: repeatElements distance [ 0 ] ++ expand (x2 :: xs)

        x :: [] ->
            [ x ]



-----------------------End Helpers------------------------------
{-
   Returns the midi note number in the range of 0..127
   C-1 to G9 or Nothing
-}


{-| Looks for a Tuple in the List "list" whose first value is "name" and if found,
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


noteAndOctaveToMidiNoteNumber : Note -> Octave -> Maybe Int
noteAndOctaveToMidiNoteNumber note octave =
    indexInList noteNames (String.toUpper note) 0
        |> Maybe.andThen
            (\nn ->
                let
                    noteNumber =
                        nn + (12 * (octave + 1))
                in
                if noteNumber < 0 || noteNumber > 127 then
                    Nothing

                else
                    Just noteNumber
            )


midiNoteNumberToString : Int -> String
midiNoteNumberToString noteNumber =
    let
        ( n, o ) =
            midiNoteNumberToNoteAndOctave noteNumber
    in
    n ++ String.fromInt o


midiNoteNumberToNoteAndOctave : Int -> ( Note, Octave )
midiNoteNumberToNoteAndOctave noteNumber =
    let
        o =
            noteNumber // 12

        n =
            modBy 12 noteNumber
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
                x :: removeAdjacentDuplicates xs

            else
                x :: removeAdjacentDuplicates (y :: xs)


expandCollection : Int -> List Int -> List Int
expandCollection times l =
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


noteCollection : List Formula -> Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
noteCollection collectionType root octave formulaName octaveRange dedup =
    let
        mnn =
            Maybe.withDefault 0 (noteAndOctaveToMidiNoteNumber root octave)

        fml =
            Maybe.withDefault [] (formula collectionType formulaName)
    in
    if dedup == False then
        fml
            |> List.map (\x -> x + mnn)
            |> expandCollection octaveRange

    else
        fml
            |> List.map (\x -> x + mnn)
            |> expandCollection octaveRange
            |> removeAdjacentDuplicates


chordCollection : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
chordCollection =
    noteCollection chordFormulaPool


scaleCollection : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
scaleCollection =
    noteCollection scaleFormulaPool


chord : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> Int -> NoteCollection
chord root octave chordName octaveRange dedup inversion =
    chordCollection root octave chordName octaveRange dedup
        |> rotateR inversion


scale : Note -> Octave -> FormulaName -> OctaveRange -> Bool -> NoteCollection
scale root octave scaleName octaveRange dedup =
    scaleCollection root octave scaleName octaveRange dedup


getNote : String -> Int -> Maybe String
getNote s i =
    if i /= 0 then
        Just s

    else
        Nothing


sequence : List String -> String -> List String
sequence notes pattern =
    let
        expandedPattern =
            listOfIntFromString pattern |> expand

        -- _ =
        --     Debug.log "In Sequence with ext pattern" expandedPattern
    in
    case notes of
        x :: xs ->
            List.filterMap identity (List.map2 getNote (x :: xs) expandedPattern)
                ++ sequence xs pattern

        _ ->
            []
