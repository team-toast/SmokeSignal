module Theme exposing (almostWhite, blue, blueButton, commonShadow, darkBlue, darkGray, darkGreen, darkRed, darkYellow, darkerBlue, disabledButton, green, inverseBlueButton, lightBlue, lightBlueButton, lightGray, lightGreen, lightRed, orange, redButton, softRed, theme, unscaryButton, veryDarkGray, yellow)

import Element exposing (Attribute, Color, Element)
import Element.Background
import Element.Border
import Element.Font
import Helpers.Element as EH


type alias Theme msg =
    { headerBackground : Color
    , headerTextColor : Color
    , appBackground : Color
    , blockBackground : Color
    , blockBorderColor : Color
    , txTrackerBackground : Color
    , postBodyBackground : Color
    , draftModalBackground : Color
    , defaultTextColor : Color
    , subtleTextColor : Color
    , linkTextColor : Color
    , linkTextColorAgainstBackground : Color
    , emphasizedTextColor : Color
    , postBodyTextColor : Color
    , messageInputPlaceholderTextColor : Color
    , loadingTextColor : Color
    , errorTextColor : Color
    , appStatusTextColor : Color
    , daiBurnedBackground : Color
    , daiBurnedTextIsWhite : Bool
    , daiTippedBackground : Color
    , daiTippedTextIsWhite : Bool
    , emphasizedActionButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
    , secondaryActionButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
    , disabledActionButton : EH.DisplayProfile -> List (Attribute msg) -> String -> Element msg
    , greenActionButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
    }


theme : Theme msg
theme =
    { headerBackground = darkBlue
    , headerTextColor = EH.white
    , appBackground = EH.black
    , blockBackground = Element.rgb 0.1 0.1 0.1
    , blockBorderColor = Element.rgb 0.25 0.25 0.25
    , postBodyBackground = lightBlue
    , draftModalBackground = darkBlue
    , txTrackerBackground = lightBlue
    , defaultTextColor = Element.rgb 0.9 0.9 0.9
    , subtleTextColor = Element.rgb 0.5 0.5 0.5
    , linkTextColor = blue
    , linkTextColorAgainstBackground = Element.rgb 0.4 0.6 1
    , emphasizedTextColor = EH.white
    , postBodyTextColor = EH.black
    , messageInputPlaceholderTextColor = darkGray
    , loadingTextColor = darkGray
    , errorTextColor = softRed
    , appStatusTextColor = darkGray
    , daiBurnedBackground = darkRed
    , daiBurnedTextIsWhite = True
    , daiTippedBackground = darkGreen
    , daiTippedTextIsWhite = True
    , emphasizedActionButton = redButton
    , secondaryActionButton = blueButton
    , disabledActionButton = disabledButton
    , greenActionButton = unscaryButton
    }



-- darkTheme : Theme msg
-- darkTheme =
--     { basicTheme
--         | appBackground = EH.black
--         , blockBackground = darkBlue
--         , mainTextColor = almostWhite
--         , emphasizedTextColor = EH.white
--         , loadingTextColor = lightGray
--         , appStatusTextColor = lightGray
--         , daiBurnedBackground = darkRed
--         , daiBurnedTextIsWhite = True
--     }


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


blueButton :
    EH.DisplayProfile
    -> List (Attribute msg)
    -> List String
    -> EH.ButtonAction msg
    -> Element msg
blueButton dProfile attributes text buttonAction =
    EH.button dProfile
        attributes
        ( Element.rgba 0 0 1 1
        , Element.rgba 0 0 1 0.8
        , Element.rgba 0 0 1 0.6
        )
        EH.white
        text
        buttonAction


lightBlueButton :
    EH.DisplayProfile
    -> List (Attribute msg)
    -> List String
    -> EH.ButtonAction msg
    -> Element msg
lightBlueButton dProfile attributes text buttonAction =
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
        buttonAction


inverseBlueButton :
    EH.DisplayProfile
    -> List (Attribute msg)
    -> List String
    -> EH.ButtonAction msg
    -> Element msg
inverseBlueButton dProfile attributes text buttonAction =
    EH.button dProfile
        attributes
        ( Element.rgba 0 0 1 0.05
        , Element.rgba 0 0 1 0.1
        , Element.rgba 0 0 1 0.2
        )
        blue
        text
        buttonAction


redButton :
    EH.DisplayProfile
    -> List (Attribute msg)
    -> List String
    -> EH.ButtonAction msg
    -> Element msg
redButton dProfile attributes text buttonAction =
    EH.button dProfile
        attributes
        ( Element.rgba 1 0 0 1
        , Element.rgba 1 0 0 0.8
        , Element.rgba 1 0 0 0.6
        )
        EH.white
        text
        buttonAction


unscaryButton :
    EH.DisplayProfile
    -> List (Attribute msg)
    -> List String
    -> EH.ButtonAction msg
    -> Element msg
unscaryButton dProfile attributes text buttonAction =
    EH.button dProfile
        attributes
        ( Element.rgb255 0 153 0
        , Element.rgba 0 1 0 0.8
        , Element.rgba 0 1 0 0.6
        )
        EH.white
        text
        buttonAction


disabledButton :
    EH.DisplayProfile
    -> List (Attribute msg)
    -> String
    -> Element msg
disabledButton dProfile attributes text =
    Element.el
        ([ Element.Border.rounded 4
         , EH.responsiveVal dProfile (Element.paddingXY 25 17) (Element.padding 10)
         , Element.Font.size (EH.responsiveVal dProfile 18 16)
         , Element.Font.semiBold
         , Element.Background.color lightGray
         , Element.Font.center
         , EH.noSelectText
         ]
            ++ attributes
        )
        (Element.el [ Element.centerY, Element.centerX ] <| Element.text text)
