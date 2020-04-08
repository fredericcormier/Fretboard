module FretboardConstants exposing
    ( fbBoardFill
    , fbBoardFretNumberAreaFill
    , fbBoardFretNumberColor
    , fbBoardOffsetX
    , fbBoardOffsetY
    , fbBoardStroke
    , fbBoardStrokeWidth
    , fbBoardWidth
    , fbBoardfretNumberHeight
    , fbDoubleInlayRadius
    , fbFingeringCircleStrokeColorNormal
    , fbFingeringCircleStrokeColorRootNote
    , fbFingeringFillColorNormal
    , fbFingeringFillColorRootNote
    , fbFingeringRadius
    , fbFingeringTextColorLight
    , fbFingeringTextColorNormal
    , fbFingeringTextColorRootNote
    , fbFretOffset
    , fbFretStroke
    , fbFretStrokeWidth
    , fbInlayFillColor
    , fbNutStrokeColor
    , fbRootFingerGreenColor
    , fbSingleInlayRadius
    , fbStringColor
    , fbStringOffset
    , fbStringSpacing
    , fbStringStart
    , fbStringStrokeBig
    , fbStringStrokeMedium
    , fbStringStrokeThick
    , fbStringStrokeThin
    )

-- renderBoard


fbBoardfretNumberHeight : Int
fbBoardfretNumberHeight =
    20


fbBoardWidth : Int
fbBoardWidth =
    860


fbBoardStroke : String
fbBoardStroke =
    "#fff"


fbBoardStrokeWidth : Int
fbBoardStrokeWidth =
    1


fbBoardFill : String
fbBoardFill =
    "#eee"


fbBoardOffsetX : Int
fbBoardOffsetX =
    0


fbBoardOffsetY : Int
fbBoardOffsetY =
    0


fbBoardFretNumberAreaFill : String
fbBoardFretNumberAreaFill =
    "#658080"


fbBoardFretNumberColor : String
fbBoardFretNumberColor =
    "#fff"



-- NUT


fbNutStrokeColor : String
fbNutStrokeColor =
    "#ddd"



-- Tuning String


fbStringStrokeThin : Int
fbStringStrokeThin =
    1


fbStringStrokeMedium : Int
fbStringStrokeMedium =
    2


fbStringStrokeThick : Int
fbStringStrokeThick =
    3


fbStringStrokeBig : Int
fbStringStrokeBig =
    5


fbStringStart : Int
fbStringStart =
    5


fbStringSpacing : Int
fbStringSpacing =
    20


fbStringOffset : Int
fbStringOffset =
    12


fbStringColor : String
fbStringColor =
    "#888"



-- FRETS


fbFretOffset : Int
fbFretOffset =
    10


fbFretStroke : String
fbFretStroke =
    "#fff"


fbFretStrokeWidth : Int
fbFretStrokeWidth =
    2



-- INLAYS


fbSingleInlayRadius : Int
fbSingleInlayRadius =
    8


fbDoubleInlayRadius : Int
fbDoubleInlayRadius =
    6


fbInlayFillColor : String
fbInlayFillColor =
    "#ddd"



-- FINGERING


fbFingeringRadius : Int
fbFingeringRadius =
    10


fbFingeringFillColorNormal : String
fbFingeringFillColorNormal =
    "#fff"


fbFingeringCircleStrokeColorNormal : String
fbFingeringCircleStrokeColorNormal =
    "#666"


fbFingeringTextColorNormal : String
fbFingeringTextColorNormal =
    "#666"


fbFingeringTextColorLight : String
fbFingeringTextColorLight =
    "#aaa"


fbFingeringFillColorRootNote : String
fbFingeringFillColorRootNote =
    "#ddd"


fbFingeringCircleStrokeColorRootNote : String
fbFingeringCircleStrokeColorRootNote =
    "#fff"


fbFingeringTextColorRootNote : String
fbFingeringTextColorRootNote =
    "#fff"


fbRootFingerGreenColor : String
fbRootFingerGreenColor =
    "#658080"
