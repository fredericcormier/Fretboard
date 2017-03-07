module Fingering exposing (..)

import Notes
    exposing
        ( NoteCollection
        , noteAndOctaveToMidiNoteNumber
        )


{- Takes a List of open Strings as MIDI NOte Number.
   For each fret of each string, mark the note as valid (it's MIDI Note Number)
   or invalid (-1)
   Returns the new list of strings (list of valid and invalid notes)

   the fingering solution has to be the same length as the string's fret count for the tuning
   otherwise the finger position won't line up with the frets
   ( some instruments have 16 frets - other 24 frets - Any number is possible )
-}


fingering : List Int -> NoteCollection -> Int -> List (List Int)
fingering openStrings validNotes fretCount =
    openStrings
        |> List.map
            (\s ->
                List.range
                    0
                    (fretCount - 1)
                    |> List.map
                        (\i ->
                            if List.member (s + i) validNotes then
                                s + i
                            else
                                -1
                        )
            )
