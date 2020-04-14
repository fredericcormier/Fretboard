port module Main exposing
    ( Model
    , Msg(..)
    , arpeggioPatterns
    , arpeggioPatternsSelect
    , audioBannerDiv
    , audioStart
    , audioStop
    , boolToChoice
    , boolToOption
    , bpmChanged
    , cellOfString
    , choiceToBool
    , doubleRootChoices
    , doubleRootSelect
    , formulaMatrixDiv
    , fretboardDiv
    , fretboardSelectionDiv
    , init
    , instrumentSelect
    , intToOption
    , main
    , matrixOfStrings
    , modeMatrixDiv
    , noteMatrixDiv
    , notesChanged
    , notesForAudio
    , notesForModelState
    , octaveMatrixDiv
    , octaveRangeSelect
    , resultMatrixDiv
    , selectionH1
    , stringToOption
    , subscriptions
    , update
    , view
    )

import Browser exposing (element)
import Exts.Html exposing (nbsp)
import Fretboard exposing (render)
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , input
        , option
        , select
        , span
        , text
        )
import Html.Attributes
    exposing
        ( class
        , classList
        , id
        , name
        , selected
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Notes
    exposing
        ( Mode
        , Note
        , Octave
        , OctaveRange
        , chord
        , chordMode
        , formulaNames
        , midiNoteNumberToString
        , noteNames
        , scale
        , scaleMode
        )
import Tuning
    exposing
        ( TuningName
        , tuningNames
        )
import WesternMusicData
    exposing
        ( FormulaName
        , chordFormulaPool
        , ionian
        , major
        , scaleFormulaPool
        )


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port audioStart : String -> Cmd msg


port audioStop : String -> Cmd msg


port bpmChanged : String -> Cmd msg


port notesChanged : List String -> Cmd msg



-- MODEL


arpeggioPatterns : List String
arpeggioPatterns =
    [ "Up"
    , "Down"
    , "Up And Down"
    , "Next Three Up"
    , "Next Three Down"
    , "Next Three Up And Down"
    , "Thirds Up"
    , "Thirds Down"
    , "Thirds Up And Down"
    , "Even"
    , "Odd"
    ]


type alias Model =
    { mode : Mode
    , note : Note
    , octave : Octave
    , formulaName : FormulaName
    , tuning : TuningName
    , range : OctaveRange
    , audioPlaying : Bool
    , arpeggioPatterns : String
    , bpm : Int
    , rootNoteDouble : Bool
    }


init : Maybe String -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { mode = scaleMode
            , note = "C"
            , octave = 2
            , formulaName = "Ionian/Major"
            , tuning = "Guitar"
            , range = 3
            , audioPlaying = False
            , arpeggioPatterns = "Up"
            , bpm = 120
            , rootNoteDouble = False
            }
    in
    ( initialModel
    , notesChanged (notesForAudio initialModel)
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = MatrixChanged String String
    | TuningChanged String
    | OctaveRangeChanged String
    | ToggleAudio
    | ArpeggioPatternChanged String
    | BPMChanged String
    | DoubleRootChanged String


doubleRootChoices : List String
doubleRootChoices =
    [ "Yes"
    , "No"
    ]



{--We need to pass the updated model to both the model variable
    and the Cmd Msg to keep the views and the audio in sync
--}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MatrixChanged m v ->
            case m of
                "Formula" ->
                    let
                        newModel =
                            { model | formulaName = v }
                    in
                    ( newModel
                    , notesChanged (notesForAudio newModel)
                    )

                "Note" ->
                    let
                        newModel =
                            { model | note = v }
                    in
                    ( newModel
                    , notesChanged (notesForAudio newModel)
                    )

                "Octave" ->
                    let
                        newModel =
                            { model | octave = Maybe.withDefault 0 (String.toInt v) }
                    in
                    ( newModel
                    , notesChanged (notesForAudio newModel)
                    )

                "Mode" ->
                    case v of
                        "Scale" ->
                            let
                                newModel =
                                    { model | mode = v, formulaName = ionian }
                            in
                            ( newModel
                            , notesChanged (notesForAudio newModel)
                            )

                        "Chord" ->
                            let
                                newModel =
                                    { model | mode = v, formulaName = major }
                            in
                            ( newModel
                            , notesChanged (notesForAudio newModel)
                            )

                        _ ->
                            ( model, notesChanged (notesForAudio model) )

                _ ->
                    ( model, notesChanged (notesForAudio model) )

        TuningChanged t ->
            ( { model | tuning = t }, Cmd.none )

        OctaveRangeChanged r ->
            let
                newModel =
                    { model | range = Maybe.withDefault 1 (String.toInt r) }
            in
            ( newModel
            , notesChanged (notesForAudio newModel)
            )

        ToggleAudio ->
            if model.audioPlaying == True then
                ( { model | audioPlaying = False }
                , audioStop "nevermind"
                )

            else
                ( { model | audioPlaying = True }
                , audioStart "nevermind"
                )

        ArpeggioPatternChanged p ->
            let
                newModel =
                    { model | arpeggioPatterns = p }
            in
            ( newModel
            , notesChanged (notesForAudio newModel)
            )

        BPMChanged newBpm ->
            ( { model | bpm = Maybe.withDefault 110 (String.toInt newBpm) }
            , bpmChanged (String.fromInt model.bpm)
            )

        DoubleRootChanged dr ->
            let
                newModel =
                    { model | rootNoteDouble = choiceToBool dr }
            in
            ( newModel
            , notesChanged (notesForAudio newModel)
            )



-- Html Select Helpers


choiceToBool : String -> Bool
choiceToBool s =
    case s of
        "Yes" ->
            True

        "No" ->
            False

        _ ->
            False


boolToChoice : Bool -> String
boolToChoice b =
    if b == True then
        "Yes"

    else
        "No"


stringToOption : String -> Html Msg
stringToOption v =
    option [ value v ] [ text v ]


intToOption : OctaveRange -> Int -> Html Msg
intToOption range i =
    option
        [ value (String.fromInt i)
        , selected (i == range)
        ]
        [ text (String.fromInt i) ]


boolToOption : Bool -> String -> Html Msg
boolToOption dedup choice =
    option
        [ value choice
        , selected (choice == boolToChoice dedup)
        ]
        [ text choice ]



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
                ++ String.fromInt model.octave
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
            (List.map (\i -> String.fromInt i) (List.range -1 9))
            (String.fromInt model.octave)
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


nextThreeUp : List int -> List int
nextThreeUp l =
    case l of
        [] ->
            []

        x :: x1 :: x2 :: x3 :: xs ->
            x :: x1 :: x2 :: x3 :: nextThreeUp (x1 :: x2 :: x3 :: xs)

        _ ->
            []


thirdsUp : List int -> List int
thirdsUp l =
    case l of
        [] ->
            []

        x :: x1 :: x2 :: x3 :: xs ->
            x :: x3 :: thirdsUp (x1 :: x2 :: x3 :: xs)

        _ ->
            []


even : List int -> List int
even l =
    case l of
        [] ->
            []

        x :: x1 :: x2 :: x3 :: x4 :: xs ->
            x2 :: x4 :: x :: even (x1 :: x2 :: x3 :: x4 :: xs)

        _ ->
            []


odd : List int -> List int
odd l =
    case l of
        [] ->
            []

        x :: x1 :: x2 :: x3 :: x4 :: x5 :: xs ->
            x3 :: x5 :: x :: odd (x1 :: x2 :: x3 :: x4 :: xs)

        _ ->
            []


notesForAudio : Model -> List String
notesForAudio model =
    let
        notes =
            notesForModelState model
    in
    case model.arpeggioPatterns of
        "Up" ->
            notes

        "Down" ->
            List.reverse notes

        "Up And Down" ->
            -- List.reverse notes |> List.append notes
            notes ++ List.reverse notes

        "Next Three Up" ->
            nextThreeUp notes

        "Next Three Down" ->
            List.reverse (nextThreeUp notes)

        "Next Three Up And Down" ->
            nextThreeUp notes ++ List.reverse (nextThreeUp notes)

        "Thirds Up" ->
            thirdsUp notes

        "Thirds Down" ->
            List.reverse (thirdsUp notes)

        "Thirds Up And Down" ->
            thirdsUp notes ++ List.reverse (thirdsUp notes)

        "Even" ->
            even notes

        "Odd" ->
            odd notes

        _ ->
            notes



{-
   retun the note collection according to the model state
-}


notesForModelState : Model -> List String
notesForModelState model =
    if model.mode == chordMode then
        chord model.note
            model.octave
            model.formulaName
            model.range
            (not model.rootNoteDouble)
            0
            |> List.take 36
            |> List.map (\i -> midiNoteNumberToString i)

    else
        scale model.note
            model.octave
            model.formulaName
            model.range
            (not model.rootNoteDouble)
            |> List.take 36
            |> List.map (\i -> midiNoteNumberToString i)


resultMatrixDiv : Model -> Html Msg
resultMatrixDiv model =
    let
        selectedCellNONE =
            ""
    in
    div
        [ classList
            [ ( "matrix", True )
            , ( "result", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Result"
            (notesForModelState model)
            selectedCellNONE
        )


instrumentSelect : Html Msg
instrumentSelect =
    div [ class "select-cell" ]
        [ select [ onInput TuningChanged, name "Intrument", class "soflow" ]
            (tuningNames |> List.map stringToOption)
        ]


octaveRangeSelect : Model -> Html Msg
octaveRangeSelect model =
    div [ class "select-cell" ]
        [ select [ onInput OctaveRangeChanged, name "Range", class "soflow" ]
            -- pass the model range as first argument so the menu can display '3' on start up
            (List.range 1 6 |> List.map (intToOption model.range))
        ]


arpeggioPatternsSelect : Html Msg
arpeggioPatternsSelect =
    div [ class "select-cell" ]
        [ select [ onInput ArpeggioPatternChanged, name "Pattern", class "soflow" ]
            (arpeggioPatterns |> List.map stringToOption)
        ]


doubleRootSelect : Model -> Html Msg
doubleRootSelect model =
    div [ class "select-cell" ]
        [ select [ onInput DoubleRootChanged, name "DoubleRoot", class "soflow" ]
            (doubleRootChoices |> List.map (boolToOption model.rootNoteDouble))
        ]


fretboardSelectionDiv : Model -> Html Msg
fretboardSelectionDiv model =
    div [ class "select-wrapper" ]
        [ div [ class "select-row" ]
            [ div [ class "select-cell-caption" ] [ text (nbsp ++ nbsp ++ "Tuning :"), instrumentSelect ]
            , div [ class "select-cell-caption" ] [ text (nbsp ++ nbsp ++ "Range :"), octaveRangeSelect model ]
            , div [ class "select-cell-caption" ] [ text (nbsp ++ nbsp ++ "Pattern :"), arpeggioPatternsSelect ]
            , div [ class "select-cell-caption" ] [ text (nbsp ++ nbsp ++ "Double Root :"), doubleRootSelect model ]
            ]
        ]


audioBannerDiv : Model -> Html Msg
audioBannerDiv model =
    div
        [ id "audio-banner"
        ]
        [ if model.audioPlaying == False then
            button [ onClick ToggleAudio ] [ text "Play" ]

          else
            button [ onClick ToggleAudio ] [ text "Stop" ]
        , input [ onInput BPMChanged, id "bpm", type_ "range", Html.Attributes.min "10", Html.Attributes.max "200" ] []
        , span [] [ text (String.fromInt model.bpm) ]
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
                    model.rootNoteDouble
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
                    model.rootNoteDouble
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
        , fretboardSelectionDiv model
        , audioBannerDiv model
        , fretboardDiv model
        ]
