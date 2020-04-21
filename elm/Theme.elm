module Theme exposing (..)

import Element exposing (Attribute, Color, Element)
import Element.Background
import Element.Border
import Element.Font
import Helpers.Element as EH


type alias Theme msg =
    { headerBackground : Color
    , appBackground : Color
    , blockBackground : Color
    , txTrackerBackground : Color
    , postBodyBackground : Color
    , draftModalBackground : Color
    , mainTextColor : Color
    , linkTextColor : Color
    , emphasizedTextColor : Color
    , postBodyTextColor : Color
    , messageInputPlaceholderTextColor : Color
    , loadingTextColor : Color
    , errorTextColor : Color
    , appStatusTextColor : Color
    , daiBurnedBackground : Color
    , daiBurnedTextIsWhite : Bool
    , emphasizedActionButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
    , secondaryActionButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
    , disabledActionButton : EH.DisplayProfile -> List (Attribute msg) -> String -> Element msg
    }


defaultTheme : Theme msg
defaultTheme =
    basicTheme


basicTheme : Theme msg
basicTheme =
    { headerBackground = darkBlue
    , appBackground = almostWhite
    , blockBackground = lightBlue
    , postBodyBackground = lightBlue
    , draftModalBackground = lightGray
    , txTrackerBackground = lightBlue
    , mainTextColor = EH.black
    , linkTextColor = blue
    , emphasizedTextColor = EH.black
    , postBodyTextColor = EH.black
    , messageInputPlaceholderTextColor = darkGray
    , loadingTextColor = darkGray
    , errorTextColor = softRed
    , appStatusTextColor = darkGray
    , daiBurnedBackground = lightRed
    , daiBurnedTextIsWhite = False
    , emphasizedActionButton = redButton
    , secondaryActionButton = blueButton
    , disabledActionButton = disabledButton
    }


darkTheme : Theme msg
darkTheme =
    { basicTheme
        | appBackground = veryDarkGray
        , blockBackground = darkBlue
        , mainTextColor = almostWhite
        , emphasizedTextColor = EH.white
        , loadingTextColor = lightGray
        , appStatusTextColor = lightGray
        , daiBurnedBackground = darkRed
        , daiBurnedTextIsWhite = True
    }


softRed =
    Element.rgb255 255 0 110


darkRed =
    Element.rgb 0.7 0 0


darkGray =
    Element.rgb255 150 150 150


blue =
    Element.rgb 0 0 1


darkBlue =
    Element.rgb255 7 27 92


lightGray =
    Element.rgb255 233 237 242


lightBlue =
    Element.rgb 0.8 0.8 1


almostWhite =
    Element.rgb 0.8 0.8 0.8


lightRed =
    Element.rgb 1 0.8 0.8


veryDarkGray =
    Element.rgb 0.1 0.1 0.1


green =
    Element.rgb255 51 183 2


yellow =
    Element.rgb 1 1 0


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



-- daiYellow =
--     yellow
-- dollarGreen =
--     green
-- placeholderTextColor =
--     Element.rgb255 213 217 222
-- mediumGray =
--     Element.rgb255 200 205 210
-- activePhaseBackgroundColor =
--     Element.rgb255 9 32 107
-- permanentTextColor =
--     Element.rgba255 1 31 52 0.8
-- submodelBackgroundColor =
--     Element.rgb 0.95 0.98 1
-- pageBackgroundColor =
--     Element.rgb255 242 243 247


blueButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
blueButton dProfile attributes text msg =
    EH.button dProfile
        attributes
        ( Element.rgba 0 0 1 1
        , Element.rgba 0 0 1 0.8
        , Element.rgba 0 0 1 0.6
        )
        EH.white
        text
        msg


lightBlueButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
lightBlueButton dProfile attributes text msg =
    let
        color =
            Element.rgb255 25 169 214
    in
    EH.button dProfile
        attributes
        ( color
        , color |> EH.withAlpha 0.8
        , color |> EH.withAlpha 0.6
        )
        EH.white
        text
        msg


inverseBlueButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
inverseBlueButton dProfile attributes text msg =
    EH.button dProfile
        attributes
        ( Element.rgba 0 0 1 0.05
        , Element.rgba 0 0 1 0.1
        , Element.rgba 0 0 1 0.2
        )
        blue
        text
        msg


redButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
redButton dProfile attributes text msg =
    EH.button dProfile
        attributes
        ( Element.rgba 1 0 0 1
        , Element.rgba 1 0 0 0.8
        , Element.rgba 1 0 0 0.6
        )
        EH.white
        text
        msg


disabledButton : EH.DisplayProfile -> List (Attribute msg) -> String -> Element msg
disabledButton dProfile attributes text =
    Element.el
        ([ Element.Border.rounded 4
         , Element.paddingXY 25 17 |> EH.changeForMobile (Element.paddingXY 10 5) dProfile
         , Element.Font.size (18 |> EH.changeForMobile 16 dProfile)
         , Element.Font.semiBold
         , Element.Background.color lightGray
         , Element.Font.center
         , EH.noSelectText
         ]
            ++ attributes
        )
        (Element.el [ Element.centerY, Element.centerX ] <| Element.text text)
