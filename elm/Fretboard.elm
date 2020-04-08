module Fretboard exposing (render)

import Fingering exposing (fingering)
import FretboardConstants exposing (..)
import Notes
    exposing
        ( midiNoteNumberToNoteAndOctave
        , noteAndOctaveToMidiNoteNumber
        )
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuning exposing (..)
import WesternMusicData exposing (NoteCollection)


shadowfilter : Svg msg
shadowfilter =
    Svg.filter [ id "shadow" ]
        [ feOffset
            [ in_ "SourceGraphic"
            , dx "1"
            , dy "1"
            , result "offOut"
            ]
            []
        , feGaussianBlur
            [ in_ "offOut"
            , stdDeviation "2"
            , result "blurOut"
            ]
            []
        , feBlend
            [ in_ "SourceGraphic"
            , in2 "blurOut"
            , mode "normal"
            ]
            []
        ]



-- MAIN BOARD


renderBoard : Board -> Svg msg
renderBoard board =
    rect
        [ fill fbBoardFill
        , stroke fbBoardStroke
        , strokeWidth (String.fromInt fbBoardStrokeWidth)
        , x (String.fromInt fbBoardOffsetX)
        , y (String.fromInt fbBoardOffsetY)
        , width (String.fromInt fbBoardWidth)
        , height (String.fromInt board.boardHeight)
        ]
        []



-- FRET NUMBERS


renderFretSideDotArea : Svg msg
renderFretSideDotArea =
    rect
        [ fill fbBoardFretNumberAreaFill
        , stroke fbBoardStroke
        , strokeWidth (String.fromInt fbBoardStrokeWidth)
        , x (String.fromInt fbBoardOffsetX)
        , y (String.fromInt fbBoardOffsetY)
        , width (String.fromInt fbBoardWidth)
        , height (String.fromInt fbBoardfretNumberHeight)
        ]
        []


renderFretSideDots : Board -> List (Svg msg)
renderFretSideDots board =
    let
        sideDots =
            [ ":", "", "", ".", "", ".", "", ".", "", ".", "", "", ":", "", "", ".", "", ".", "", ".", "", ".", "", "", "" ]

        radius =
            2

        fretOffset =
            2
    in
    sideDots
        |> List.indexedMap
            (\index n ->
                case n of
                    "." ->
                        circle
                            [ cx (String.fromInt ((index * board.fretSpacing) + (board.fretSpacing // fretOffset)))
                            , cy (String.fromInt 10)
                            , r (String.fromInt radius)
                            , fill fbBoardFretNumberColor
                            ]
                            []

                    ":" ->
                        svg
                            []
                            [ circle
                                [ cx (String.fromInt ((index * board.fretSpacing) + (board.fretSpacing // fretOffset)))
                                , cy (String.fromInt 7)
                                , r (String.fromInt radius)
                                , fill fbBoardFretNumberColor
                                ]
                                []
                            , circle
                                [ cx (String.fromInt ((index * board.fretSpacing) + (board.fretSpacing // fretOffset)))
                                , cy (String.fromInt 13)
                                , r (String.fromInt radius)
                                , fill fbBoardFretNumberColor
                                ]
                                []
                            ]

                    _ ->
                        circle [] []
            )



-- NUT


renderNut : Board -> Svg msg
renderNut board =
    line
        [ x1 "0"
        , y1 "0"
        , x2 "0"
        , y2 (String.fromInt board.boardHeight)
        , strokeWidth (String.fromInt 4)
        , stroke fbNutStrokeColor
        ]
        []



-- FRETS


renderFrets : Board -> List (Svg msg)
renderFrets board =
    List.range 1 board.fretCount
        |> List.map
            (\i -> fret i board)


fret : Int -> Board -> Svg msg
fret fretNumber board =
    line
        [ x1 (String.fromInt (fretNumber * board.fretSpacing))
        , y1 (String.fromInt fbBoardfretNumberHeight)
        , x2 (String.fromInt (fretNumber * board.fretSpacing))
        , y2 (String.fromInt board.boardHeight)
        , strokeWidth "2"
        , stroke fbFretStroke
        ]
        []



-- INLAYS


type InlayType
    = Single
    | Double


renderInlays : Board -> List (Svg msg)
renderInlays board =
    List.range 1 board.fretCount
        |> List.map
            (\i ->
                -- "+ 1" is because of fret zero/open string
                if List.member (i + 1) [ 3 + 1, 5 + 1, 7 + 1, 9 + 1, 15 + 1, 17 + 1, 19 + 1, 21 + 1 ] then
                    -- Don't draw inlays if fretCount == 16 and fret >= 16
                    if board.fretCount == 16 && (i + 1) >= 16 then
                        circle [] []

                    else
                        inlay i board Single
                    -- "+ 1" is because of fret zero/open string

                else if List.member (i + 1) [ 12 + 1, 24 + 1 ] then
                    inlay i board Double

                else
                    circle [] []
            )


inlay : Int -> Board -> InlayType -> Svg msg
inlay fretNumber board inlayPosition =
    case inlayPosition of
        Single ->
            circle
                [ cx (String.fromInt ((fretNumber * board.fretSpacing) + (board.fretSpacing // 2)))
                , cy (String.fromInt ((board.boardHeight + fbBoardfretNumberHeight) // 2))
                , r (String.fromInt fbSingleInlayRadius)
                , fill fbInlayFillColor
                ]
                []

        Double ->
            -- an function  svg let me return 2 shapes in one 'Svg msg'.
            -- Don't specify the size of this nested svg
            -- it will cause the nested svg not to display properly
            svg
                []
                [ circle
                    [ cx (String.fromInt ((fretNumber * board.fretSpacing) + (board.fretSpacing // 2)))
                    , cy (String.fromInt ((board.boardHeight + fbBoardfretNumberHeight) // 4))
                    , r (String.fromInt fbDoubleInlayRadius)
                    , fill fbInlayFillColor
                    ]
                    []
                , circle
                    [ cx (String.fromInt ((fretNumber * board.fretSpacing) + (board.fretSpacing // 2)))
                    , cy (String.fromInt (((board.boardHeight + fbBoardfretNumberHeight) // 4) * 3))
                    , r (String.fromInt fbDoubleInlayRadius)
                    , fill fbInlayFillColor
                    ]
                    []
                ]



-- STRINGS


renderStrings : String -> Board -> List (Svg msg)
renderStrings instrumentName board =
    gauges tuningPool instrumentName
        |> List.indexedMap (\i g -> string i g board)


string : Int -> Int -> Board -> Svg msg
string stringNumber thickness board =
    line
        [ x1 (String.fromInt board.fretSpacing)
        , y1 (String.fromInt (fbBoardfretNumberHeight + fbStringOffset + (fbStringSpacing * stringNumber)))
        , x2 (String.fromInt fbBoardWidth)
        , y2 (String.fromInt (fbBoardfretNumberHeight + fbStringOffset + (fbStringSpacing * stringNumber)))
        , strokeWidth (String.fromInt thickness)
        , stroke fbStringColor
        ]
        []



--OPEN STRINGS


renderOpenStringNoteNames : Board -> List (Svg msg)
renderOpenStringNoteNames board =
    board.openStrings
        |> List.indexedMap
            (\i n ->
                let
                    ( noteName, _, _ ) =
                        n
                in
                oneFinger
                    i
                    0
                    (noteName |> String.toUpper)
                    fbBoardFill
                    fbBoardFill
                    fbFingeringTextColorLight
                    ""
                    board
            )



-- FINGERINGS


renderFingerings : Board -> List (Svg msg)
renderFingerings board =
    board.fingering
        |> List.indexedMap
            (\i s -> oneStringFingering i s board)
        |> List.concat


oneStringFingering : Int -> List Int -> Board -> List (Svg msg)
oneStringFingering stringNumber list board =
    list
        |> List.indexedMap
            (\i noteAtFret ->
                if noteAtFret == -1 then
                    circle [] []

                else
                    let
                        ( noteName, _ ) =
                            Notes.midiNoteNumberToNoteAndOctave noteAtFret

                        ( rootNoteName, _ ) =
                            Notes.midiNoteNumberToNoteAndOctave board.rootNoteMDINumber
                    in
                    if noteName == rootNoteName then
                        oneFinger stringNumber
                            i
                            noteName
                            fbFingeringFillColorRootNote
                            fbFingeringCircleStrokeColorNormal
                            fbFingeringTextColorNormal
                            ""
                            board

                    else
                        oneFinger stringNumber
                            i
                            noteName
                            fbFingeringFillColorNormal
                            fbFingeringCircleStrokeColorNormal
                            fbFingeringTextColorNormal
                            "url(#shadow)"
                            board
            )


oneFinger : Int -> Int -> String -> String -> String -> String -> String -> Board -> Svg msg
oneFinger stringNumber index noteName fillColor strokeColor textColor filterURL board =
    svg []
        [ circle
            [ cx (String.fromInt ((index * board.fretSpacing) + (board.fretSpacing // 2)))
            , cy (String.fromInt ((stringNumber * fbStringSpacing) + fbStringOffset + fbBoardfretNumberHeight))
            , r (String.fromInt fbFingeringRadius)
            , fill fillColor
            , stroke strokeColor
            , Svg.Attributes.filter filterURL
            ]
            []
        , text_
            [ textAnchor "middle"
            , stroke textColor
            , x (String.fromInt ((index * board.fretSpacing) + (board.fretSpacing // 2)))
            , y (String.fromInt ((stringNumber * fbStringSpacing) + fbStringOffset + 3 + fbBoardfretNumberHeight))
            , fontWeight "200"
            , fontSize "10px"
            ]
            [ text noteName ]
        ]



-- Main stuff


type alias Board =
    { openStrings : List IString
    , stringCount : Int
    , boardHeight : Int
    , fretCount : Int
    , fretSpacing : Int
    , rootNoteMDINumber : Int
    , fingering : List (List Int)
    }



--[(openstring name, octave fretcount), … ]


openStrings : String -> List IString
openStrings instrumentName =
    Tuning.strings tuningPool instrumentName


stringCount : String -> Int
stringCount instrumentName =
    List.length (openStrings instrumentName)


boardHeight : String -> Int
boardHeight instrumentName =
    (stringCount instrumentName - 1) * fbStringSpacing + (fbStringOffset * 2) + fbBoardfretNumberHeight



-- All the strings at fret 0


openStringMidiNoteNumber : String -> List Int
openStringMidiNoteNumber instrumentName =
    openStrings instrumentName
        |> List.map
            (\( n, o, _ ) -> Maybe.withDefault 0 (Notes.noteAndOctaveToMidiNoteNumber n o))


rootNoteMDINumber : NoteCollection -> Int
rootNoteMDINumber noteCollection =
    Maybe.withDefault 0 (List.head noteCollection)


fingering : String -> NoteCollection -> Int -> List (List Int)
fingering instrumentName noteCollection fretCount =
    Fingering.fingering (openStringMidiNoteNumber instrumentName) noteCollection fretCount



{-
   Main Stuff
-}


render : String -> NoteCollection -> Svg msg
render instrumentName noteCollection =
    let
        ( _, _, fretCount ) =
            openStrings instrumentName
                |> List.head
                |> Maybe.withDefault ( "", 0, 16 )

        board : Board
        board =
            { openStrings = openStrings instrumentName
            , stringCount = stringCount instrumentName
            , boardHeight = boardHeight instrumentName
            , fretCount = fretCount
            , fretSpacing = fbBoardWidth // fretCount
            , rootNoteMDINumber = rootNoteMDINumber noteCollection
            , fingering = fingering instrumentName noteCollection fretCount
            }
    in
    svg
        [ width "860"
        , height "280"
        , viewBox "0 0 860 280"
        ]
        (renderFingerings board
            |> List.append
                (renderOpenStringNoteNames board
                    |> List.append
                        (renderStrings instrumentName board
                            |> List.append
                                (renderInlays board
                                    |> List.append
                                        (renderFretSideDots board
                                            |> List.append
                                                (renderFrets board
                                                    |> List.append
                                                        [ shadowfilter
                                                        , renderBoard board
                                                        , renderNut board
                                                        , renderFretSideDotArea
                                                        ]
                                                )
                                        )
                                )
                        )
                )
        )
