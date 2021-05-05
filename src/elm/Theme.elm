module Theme exposing (almostWhite, black, blackAlpha, blue, commonShadow, darkBlue, darkGray, darkGreen, darkRed, darkYellow, darkerBlue, ethereum, green, lightBlue, lightGray, lightGreen, lightRed, orange, softRed, veryDarkGray, white, withAlpha, xDai, yellow)

import Element exposing (Attribute, Color, rgb255)
import Element.Border
import Types exposing (DisplayProfile(..))


white : Color
white =
    Element.rgb255 255 255 255


black : Color
black =
    Element.rgb255 0 0 0


blackAlpha : Float -> Color
blackAlpha =
    Element.rgba255 0 0 0


xDai : Color
xDai =
    rgb255 7 132 112


ethereum : Color
ethereum =
    rgb255 90 110 249


orange : Color
orange =
    Element.rgb255 247 155 48


softRed : Color
softRed =
    Element.rgb255 255 0 110


darkRed : Color
darkRed =
    Element.rgb 0.7 0 0


darkGray : Color
darkGray =
    Element.rgb255 150 150 150


blue : Color
blue =
    Element.rgb 0 0 1


darkBlue : Color
darkBlue =
    Element.rgb255 7 27 92


darkerBlue : Color
darkerBlue =
    Element.rgb255 7 20 60


lightGray : Color
lightGray =
    Element.rgb255 233 237 242


lightBlue : Color
lightBlue =
    Element.rgb 0.8 0.8 1


almostWhite : Color
almostWhite =
    Element.rgb 0.8 0.8 0.8


lightRed : Color
lightRed =
    Element.rgb 1 0.8 0.8


lightGreen : Color
lightGreen =
    Element.rgb 0.8 1 0.8


veryDarkGray : Color
veryDarkGray =
    Element.rgb 0.1 0.1 0.1


green : Color
green =
    Element.rgb255 51 183 2


darkGreen : Color
darkGreen =
    Element.rgb255 0 120 0


yellow : Color
yellow =
    Element.rgb 1 1 0


darkYellow : Color
darkYellow =
    Element.rgb 0.6 0.6 0


commonShadow : Attribute msg
commonShadow =
    Element.Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = darkGray
        }


withAlpha : Float -> Color -> Color
withAlpha a color =
    let
        oldRgba =
            Element.toRgb color
    in
    Element.fromRgb
        { oldRgba
            | alpha = a
        }
