module Fretboard exposing (render)

import FretboardConstants exposing (..)
import Notes
    exposing
        ( midiNoteNumberToNoteAndOctave
        , noteAndOctaveToMidiNoteNumber
        )
import WesternMusicData exposing (NoteCollection)
import Tuning exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Fingering exposing (fingering)


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
        , strokeWidth (fbBoardStrokeWidth |> toString)
        , x (fbBoardOffsetX |> toString)
        , y (fbBoardOffsetY |> toString)
        , width (fbBoardWidth |> toString)
        , height (toString board.boardHeight)
        ]
        []



-- FRET NUMBERS


renderFretSideDotArea : Board -> Svg msg
renderFretSideDotArea board =
    rect
        [ fill fbBoardFretNumberAreaFill
        , stroke fbBoardStroke
        , strokeWidth (fbBoardStrokeWidth |> toString)
        , x (fbBoardOffsetX |> toString)
        , y (fbBoardOffsetY |> toString)
        , width (fbBoardWidth |> toString)
        , height (fbBoardfretNumberHeight |> toString)
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
                                [ cx ((((index) * board.fretSpacing) + (board.fretSpacing // fretOffset)) |> toString)
                                , cy (10 |> toString)
                                , r (radius |> toString)
                                , fill fbBoardFretNumberColor
                                ]
                                []

                        ":" ->
                            svg
                                []
                                [ circle
                                    [ cx (((index * board.fretSpacing) + (board.fretSpacing // fretOffset)) |> toString)
                                    , cy (7 |> toString)
                                    , r (radius |> toString)
                                    , fill fbBoardFretNumberColor
                                    ]
                                    []
                                , circle
                                    [ cx (((index * board.fretSpacing) + (board.fretSpacing // fretOffset)) |> toString)
                                    , cy (13 |> toString)
                                    , r (radius |> toString)
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
        , y2 (board.boardHeight |> toString)
        , strokeWidth (4 |> toString)
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
        [ x1 ((fretNumber * board.fretSpacing) |> toString)
        , y1 (fbBoardfretNumberHeight |> toString)
        , x2 ((fretNumber * board.fretSpacing) |> toString)
        , y2 (board.boardHeight |> toString)
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
                    (inlay i board Double)
                else
                    circle [] []
            )


inlay : Int -> Board -> InlayType -> Svg msg
inlay fretNumber board inlayPosition =
    case inlayPosition of
        Single ->
            circle
                [ cx (((fretNumber * board.fretSpacing) + (board.fretSpacing // 2)) |> toString)
                , cy (((board.boardHeight + fbBoardfretNumberHeight) // 2) |> toString)
                , r (fbSingleInlayRadius |> toString)
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
                    [ cx (((fretNumber * board.fretSpacing) + (board.fretSpacing // 2)) |> toString)
                    , cy ((((board.boardHeight + fbBoardfretNumberHeight) // 4)) |> toString)
                    , r (fbDoubleInlayRadius |> toString)
                    , fill fbInlayFillColor
                    ]
                    []
                , circle
                    [ cx (((fretNumber * board.fretSpacing) + (board.fretSpacing // 2)) |> toString)
                    , cy ((((board.boardHeight + fbBoardfretNumberHeight) // 4) * 3) |> toString)
                    , r (fbDoubleInlayRadius |> toString)
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
        [ x1 (board.fretSpacing |> toString)
        , y1 (fbBoardfretNumberHeight + fbStringOffset + (fbStringSpacing * stringNumber) |> toString)
        , x2 (fbBoardWidth |> toString)
        , y2 (fbBoardfretNumberHeight + fbStringOffset + (fbStringSpacing * stringNumber) |> toString)
        , strokeWidth (thickness |> toString)
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
                    (oneFinger
                        i
                        0
                        (noteName |> String.toUpper)
                        fbBoardFill
                        fbBoardFill
                        fbFingeringTextColorLight
                        ""
                        board
                    )
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
            [ cx ((((index) * board.fretSpacing) + (board.fretSpacing // 2)) |> toString)
            , cy (((stringNumber * fbStringSpacing) + fbStringOffset + fbBoardfretNumberHeight) |> toString)
            , r (fbFingeringRadius |> toString)
            , fill fillColor
            , stroke strokeColor
            , Svg.Attributes.filter filterURL
            ]
            []
        , text_
            [ textAnchor "middle"
            , stroke textColor
            , x ((((index) * board.fretSpacing) + (board.fretSpacing // 2)) |> toString)
            , y (((stringNumber * fbStringSpacing) + fbStringOffset + 3 + fbBoardfretNumberHeight) |> toString)
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
    (Tuning.strings tuningPool instrumentName)


stringCount : String -> Int
stringCount instrumentName =
    List.length (openStrings instrumentName)


boardHeight : String -> Int
boardHeight instrumentName =
    ((stringCount instrumentName) - 1) * fbStringSpacing + (fbStringOffset * 2) + fbBoardfretNumberHeight



-- All the strings at fret 0


openStringMidiNoteNumber : String -> List Int
openStringMidiNoteNumber instrumentName =
    (openStrings instrumentName)
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
            (openStrings instrumentName)
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
            ((renderFingerings board)
                |> List.append
                    ((renderOpenStringNoteNames board)
                        |> List.append
                            ((renderStrings instrumentName board)
                                |> List.append
                                    ((renderInlays board)
                                        |> List.append
                                            ((renderFretSideDots board)
                                                |> List.append
                                                    ((renderFrets board)
                                                        |> List.append
                                                            [ shadowfilter
                                                            , renderBoard board
                                                            , renderNut board
                                                            , renderFretSideDotArea board
                                                            ]
                                                    )
                                            )
                                    )
                            )
                    )
            )
