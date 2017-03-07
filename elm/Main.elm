module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Notes exposing (..)
import Tuning exposing (..)
import Fretboard exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { mode : Mode
    , note : Note
    , octave : Octave
    , formulaName : FormulaName
    , tuning : TuningName
    , range : OctaveRange
    }


model : Model
model =
    { mode = scaleMode
    , note = "C"
    , octave = 2
    , formulaName = ionian
    , tuning = "Guitar"
    , range = 3
    }



-- UPDATE


type Msg
    = MatrixChanged String String
    | TuningChanged String
    | OctaveRangeChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        MatrixChanged m v ->
            case m of
                "Formula" ->
                    { model | formulaName = v }

                "Note" ->
                    { model | note = v }

                "Octave" ->
                    { model | octave = Result.withDefault 0 (String.toInt v) }

                "Mode" ->
                    case v of
                        "Scale" ->
                            { model
                                | mode = v
                                , formulaName =
                                    ionian
                            }

                        "Chord" ->
                            { model
                                | mode = v
                                , formulaName =
                                    major
                            }

                        _ ->
                            model

                _ ->
                    model

        TuningChanged t ->
            { model | tuning = t }

        OctaveRangeChanged r ->
            { model | range = Result.withDefault 1 (String.toInt r) }



-- Html Select Helpers
-- put any select option here


stringToOption : String -> Html Msg
stringToOption v =
    option [ value v ] [ text v ]


intToOption : Int -> Html Msg
intToOption i =
    option
        [ value (toString i)
        , selected (i == model.range)
        ]
        [ text (toString i) ]



-- Matrices of Div's


cellOfString : String -> String -> String -> Html Msg
cellOfString matrixName thisCellName selectedCellName =
    div
        [ onClick (MatrixChanged matrixName thisCellName)
        , classList
            [ ( "cell", True )
            , ( String.toLower matrixName, True )
            , ( "cell-selected", thisCellName == selectedCellName )
            ]
        ]
        [ text thisCellName ]


matrixOfStrings : String -> List String -> String -> List (Html Msg)
matrixOfStrings matrixName theList theSelectedItem =
    List.map (\s -> cellOfString matrixName s theSelectedItem) theList



-- VIEWS


modeMatrixDiv : Model -> Html Msg
modeMatrixDiv model =
    div
        [ classList
            [ ( "matrix", True )
            , ( "mode", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Mode" [ scaleMode, chordMode ] model.mode)


selectionH1 : Model -> Html Msg
selectionH1 model =
    h1 []
        [ text <|
            model.note
                ++ " "
                ++ (toString model.octave)
                ++ " - "
                ++ model.formulaName
        ]


noteMatrixDiv : Model -> Html Msg
noteMatrixDiv model =
    div
        [ classList
            [ ( "matrix", True )
            , ( "note", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Note" noteNames model.note)


octaveMatrixDiv : Model -> Html Msg
octaveMatrixDiv model =
    div
        [ classList
            [ ( "matrix", True )
            , ( "octave", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Octave"
            (List.map (\i -> toString (i)) (List.range -1 9))
            (toString (model.octave))
        )


formulaMatrixDiv : Model -> Html Msg
formulaMatrixDiv model =
    if model.mode == chordMode then
        div
            [ classList
                [ ( "matrix", True )
                , ( "pattern", True )
                , ( "inline-block", True )
                ]
            ]
            (matrixOfStrings "Formula" (formulaNames chordFormulaPool) model.formulaName)
    else
        div
            [ classList
                [ ( "matrix", True )
                , ( "pattern", True )
                , ( "inline-block", True )
                ]
            ]
            (matrixOfStrings "Formula" (formulaNames scaleFormulaPool) model.formulaName)


resultMatrixDiv : Model -> Html Msg
resultMatrixDiv model =
    let
        selectedCellNONE =
            ""
    in
        if model.mode == chordMode then
            div
                [ classList
                    [ ( "matrix", True )
                    , ( "result", True )
                    , ( "inline-block", True )
                    ]
                ]
                (matrixOfStrings "Result"
                    ((chord model.note
                        model.octave
                        model.formulaName
                        model.range
                        0
                     )
                        |> List.take 20
                        |> List.map (\i -> midiNoteNumberToString (i))
                    )
                    selectedCellNONE
                )
        else
            div
                [ classList
                    [ ( "matrix", True )
                    , ( "result", True )
                    , ( "inline-block", True )
                    ]
                ]
                (matrixOfStrings "Result"
                    ((scale model.note
                        model.octave
                        model.formulaName
                        model.range
                     )
                        |> List.take 20
                        |> List.map (\i -> midiNoteNumberToString (i))
                    )
                    selectedCellNONE
                )


selectDiv : Model -> Html Msg
selectDiv model =
    div [ class "select-wrapper" ]
        [ div [ class "select-row" ]
            [ div [ class "select-cell" ] [ text "Tuning :" ]
            , instrumentSelect model
            , div [ class "select-cell" ] [ text "Octave Range :" ]
            , octaveRangeSelect model
            ]
        ]


instrumentSelect : Model -> Html Msg
instrumentSelect model =
    div [ class "select-cell" ]
        [ select [ onInput TuningChanged, name "Intrument", class "soflow" ]
            (names |> List.map stringToOption)
        ]


octaveRangeSelect : Model -> Html Msg
octaveRangeSelect model =
    div [ class "select-cell" ]
        [ select [ onInput OctaveRangeChanged, name "Range", class "soflow" ]
            ((List.range 1 6) |> List.map intToOption)
        ]


fretboardDiv : Model -> Html Msg
fretboardDiv model =
    if model.mode == scaleMode then
        div
            [ id "fretboard"
            ]
            [ Fretboard.render model.tuning
                (scale model.note
                    model.octave
                    model.formulaName
                    model.range
                )
            ]
    else
        div
            [ id "fretboard"
            ]
            [ Fretboard.render model.tuning
                (chord model.note
                    model.octave
                    model.formulaName
                    model.range
                    0
                )
            ]


view : Model -> Html Msg
view model =
    div
        [ class "centered-wrapper" ]
        [ modeMatrixDiv model
        , selectionH1 model
        , noteMatrixDiv model
        , octaveMatrixDiv model
        , formulaMatrixDiv model
        , resultMatrixDiv model
        , selectDiv model
        , fretboardDiv model
        ]
