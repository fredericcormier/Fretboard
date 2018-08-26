module Tuning exposing
    ( IString
    , Tuning
    , TuningName
    , gauges
    , strings
    , tuningNames
    , tuningPool
    )


stringGaugeThin =
    1


stringGaugeMedium =
    2


stringGaugeThick =
    3


stringGaugeBig =
    5



-- IString stands for Instrument String
-- name, octave, fretcount


type alias IString =
    ( String, Int, Int )


type alias TuningName =
    String



-- Tuning has a name, List of open strings and a list of gauges


type alias Tuning =
    ( TuningName, List IString, List Gauge )


type alias Gauge =
    Int



-- returns all the tuning names in the pool


tuningNames : List TuningName
tuningNames =
    tuningPool
        |> List.map (\( n, t, g ) -> n)



-- tuning is a recursive function thus the List parameter:
-- t = tuning tuningPool "Guitar"
-- tuning returns all the open strings for this instrument's tuning


strings : List Tuning -> String -> List IString
strings l name =
    case l of
        [] ->
            []

        ( n, t, g ) :: xs ->
            if n == name then
                t

            else
                strings xs name


gauges : List Tuning -> String -> List Gauge
gauges l name =
    case l of
        [] ->
            []

        ( n, t, g ) :: xs ->
            if n == name then
                g

            else
                gauges xs name


tuningPool : List Tuning
tuningPool =
    [ ( "Guitar"
      , [ ( "e", 4, 24 )
        , ( "b", 3, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 2, 24 )
        , ( "e", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Drop D"
      , [ ( "e", 4, 24 )
        , ( "b", 3, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 2, 24 )
        , ( "d", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Double Drop D"
      , [ ( "d", 4, 24 )
        , ( "b", 3, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 2, 24 )
        , ( "d", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Drop C"
      , [ ( "d", 4, 24 )
        , ( "a", 3, 24 )
        , ( "f", 3, 24 )
        , ( "c", 3, 24 )
        , ( "g", 2, 24 )
        , ( "c", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Drop B"
      , [ ( "c#", 4, 24 )
        , ( "g#", 3, 24 )
        , ( "e", 3, 24 )
        , ( "b", 2, 24 )
        , ( "f#", 2, 24 )
        , ( "b", 1, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Eb"
      , [ ( "d#", 4, 24 )
        , ( "a#", 3, 24 )
        , ( "f#", 3, 24 )
        , ( "c#", 3, 24 )
        , ( "g#", 2, 24 )
        , ( "d#", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar D"
      , [ ( "d", 4, 24 )
        , ( "a", 3, 24 )
        , ( "f", 3, 24 )
        , ( "c", 3, 24 )
        , ( "g", 2, 24 )
        , ( "d", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar C"
      , [ ( "c", 4, 24 )
        , ( "g", 3, 24 )
        , ( "d#", 3, 24 )
        , ( "a#", 3, 24 )
        , ( "f", 2, 24 )
        , ( "c", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Open D"
      , [ ( "d", 4, 24 )
        , ( "a", 3, 24 )
        , ( "f#", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 2, 24 )
        , ( "d", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Open G"
      , [ ( "d", 4, 24 )
        , ( "b", 3, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "g", 2, 24 )
        , ( "d", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar D Modal"
      , [ ( "d", 4, 24 )
        , ( "a", 3, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 2, 24 )
        , ( "d", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Open A"
      , [ ( "e", 4, 24 )
        , ( "a", 3, 24 )
        , ( "e", 3, 24 )
        , ( "c#", 3, 24 )
        , ( "a", 2, 24 )
        , ( "e", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Open E"
      , [ ( "e", 4, 24 )
        , ( "b", 3, 24 )
        , ( "g#", 3, 24 )
        , ( "e", 3, 24 )
        , ( "b", 2, 24 )
        , ( "e", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar Open C"
      , [ ( "e", 4, 24 )
        , ( "c", 3, 24 )
        , ( "g", 3, 24 )
        , ( "c", 3, 24 )
        , ( "g", 2, 24 )
        , ( "c", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar NST"
      , [ ( "g", 4, 24 )
        , ( "e", 4, 24 )
        , ( "a", 3, 24 )
        , ( "d", 3, 24 )
        , ( "g", 2, 24 )
        , ( "c", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        ]
      )
    , ( "Guitar 8 String Standard"
      , [ ( "e", 4, 24 )
        , ( "b", 3, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 2, 24 )
        , ( "e", 2, 24 )
        , ( "b", 1, 24 )
        , ( "f#", 1, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        , stringGaugeThick
        , stringGaugeBig
        ]
      )
    , ( "Guitar 8 String Drop E"
      , [ ( "e", 4, 24 )
        , ( "b", 3, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 2, 24 )
        , ( "e", 2, 24 )
        , ( "b", 1, 24 )
        , ( "e", 1, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        , stringGaugeThick
        , stringGaugeBig
        ]
      )
    , ( "Bass"
      , [ ( "g", 2, 24 )
        , ( "d", 2, 24 )
        , ( "a", 1, 24 )
        , ( "e", 1, 24 )
        ]
      , [ stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        , stringGaugeBig
        ]
      )
    , ( "Bass 6 Strings"
      , [ ( "c", 3, 24 )
        , ( "g", 2, 24 )
        , ( "d", 2, 24 )
        , ( "a", 1, 24 )
        , ( "e", 1, 24 )
        , ( "b", 0, 24 )
        ]
      , [ stringGaugeMedium
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        , stringGaugeBig
        , stringGaugeBig
        ]
      )
    , ( "Cello"
      , [ ( "a", 2, 16 )
        , ( "d", 2, 16 )
        , ( "g", 1, 16 )
        , ( "c", 1, 16 )
        ]
      , [ stringGaugeMedium
        , stringGaugeThick
        , stringGaugeThick
        , stringGaugeBig
        ]
      )
    , ( "Mandola"
      , [ ( "a", 3, 16 )
        , ( "d", 3, 16 )
        , ( "g", 2, 16 )
        , ( "c", 2, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        ]
      )
    , ( "Banjo Irish"
      , [ ( "e", 4, 16 )
        , ( "a", 3, 16 )
        , ( "d", 3, 16 )
        , ( "g", 2, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        ]
      )
    , ( "Banjo Tenor"
      , [ ( "a", 4, 16 )
        , ( "d", 4, 16 )
        , ( "g", 3, 16 )
        , ( "c", 3, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        ]
      )
    , ( "Banjo Chicago"
      , [ ( "e", 4, 16 )
        , ( "b", 3, 16 )
        , ( "g", 3, 16 )
        , ( "d", 3, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        ]
      )
    , ( "Violin"
      , [ ( "e", 5, 16 )
        , ( "a", 4, 16 )
        , ( "d", 4, 16 )
        , ( "g", 3, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        ]
      )
    , ( "Viola"
      , [ ( "a", 5, 16 )
        , ( "d", 4, 16 )
        , ( "g", 4, 16 )
        , ( "c", 3, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeMedium
        , stringGaugeMedium
        , stringGaugeMedium
        ]
      )
    , ( "Ukelele D"
      , [ ( "b", 4, 16 )
        , ( "f#", 4, 16 )
        , ( "d", 4, 16 )
        , ( "a", 3, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        ]
      )
    , ( "Ukelele C"
      , [ ( "a", 4, 16 )
        , ( "e", 4, 16 )
        , ( "c", 4, 16 )
        , ( "g", 3, 16 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        ]
      )
    , ( "Stick Left Hand Classic"
      , [ ( "c", 1, 24 )
        , ( "g", 1, 24 )
        , ( "d", 2, 24 )
        , ( "a", 2, 24 )
        , ( "e", 3, 24 )
        , ( "b", 3, 24 )
        ]
      , [ stringGaugeBig
        , stringGaugeThick
        , stringGaugeMedium
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        ]
      )
    , ( "Stick RightHand Classic"
      , [ ( "d", 4, 24 )
        , ( "a", 3, 24 )
        , ( "e", 3, 24 )
        , ( "b", 3, 24 )
        , ( "f#", 2, 24 )
        , ( "c#", 2, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        ]
      )
    , ( "Stick 12 Classic"
      , [ ( "d", 4, 24 )
        , ( "a", 3, 24 )
        , ( "e", 3, 24 )
        , ( "b", 3, 24 )
        , ( "f#", 2, 24 )
        , ( "c#", 2, 24 )
        , ( "c", 1, 24 )
        , ( "g", 1, 24 )
        , ( "d", 2, 24 )
        , ( "a", 2, 24 )
        , ( "e", 3, 24 )
        , ( "b", 3, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeBig
        , stringGaugeThick
        , stringGaugeMedium
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        ]
      )
    , ( "Stick 12 Matched Reciprocal"
      , [ ( "c", 4, 24 )
        , ( "g", 3, 24 )
        , ( "d", 3, 24 )
        , ( "a", 3, 24 )
        , ( "e", 2, 24 )
        , ( "b", 2, 24 )
        , ( "c", 1, 24 )
        , ( "g", 1, 24 )
        , ( "d", 2, 24 )
        , ( "a", 2, 24 )
        , ( "e", 3, 24 )
        , ( "b", 3, 24 )
        ]
      , [ stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeMedium
        , stringGaugeThick
        , stringGaugeBig
        , stringGaugeThick
        , stringGaugeMedium
        , stringGaugeThin
        , stringGaugeThin
        , stringGaugeThin
        ]
      )

    -- END OF POOOL
    ]
