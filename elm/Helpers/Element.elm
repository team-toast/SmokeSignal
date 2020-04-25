module Helpers.Element exposing (..)

import Browser.Dom
import Collage exposing (Collage)
import Collage.Render
import Color exposing (Color)
import Config
import Css
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Html.Attributes
import Html.Events
import Html.Styled
import Json.Decode
import List
import List.Extra
import Maybe.Extra
import Task
import Time
import TokenValue exposing (TokenValue)


forgedByFoundry : DisplayProfile -> Element msg
forgedByFoundry dProfile =
    Element.newTabLink
        [ Element.padding (8 |> changeForMobile 4 dProfile)
        , Element.Background.color <| Element.rgb 0.3 0.3 0.3
        , Element.Border.rounded (5 |> changeForMobile 3 dProfile)
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.rounded 5
                , Element.Border.innerShadow
                    { offset = ( 2, 2 )
                    , size = 0
                    , blur = 5
                    , color = Element.rgba 1 1 1 0.4
                    }
                ]
                Element.none
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.rounded 5
                , Element.Border.innerShadow
                    { offset = ( -2, -2 )
                    , size = 0
                    , blur = 5
                    , color = Element.rgba 0 0 0 0.4
                    }
                ]
                Element.none
        ]
        { url = "https://foundrydao.com/"
        , label =
            Element.column
                [ Element.spacing 4
                ]
                [ Element.el
                    [ Element.Font.color white
                    , Element.Font.size (20 |> changeForMobile 14 dProfile)
                    ]
                  <|
                    Element.text "Forged by"
                , Element.el
                    [ Element.Font.size (22 |> changeForMobile 16 dProfile)
                    , Element.Font.bold
                    , Element.Font.color <| Element.rgb255 255 0 110
                    ]
                  <|
                    Element.text "Foundry"
                ]
        }


type DisplayProfile
    = Desktop
    | Mobile


screenWidthToDisplayProfile : Int -> DisplayProfile
screenWidthToDisplayProfile width =
    if width >= 1150 then
        Desktop

    else
        Mobile


changeForMobile : a -> DisplayProfile -> a -> a
changeForMobile changed dProfile original =
    case dProfile of
        Desktop ->
            original

        Mobile ->
            changed



-- COLORS


transparent =
    Element.rgba 0 0 0 0


black =
    Element.rgb 0 0 0


white =
    Element.rgb 1 1 1


withAlpha : Float -> Element.Color -> Element.Color
withAlpha a color =
    let
        oldRgba =
            Element.toRgb color
    in
    Element.fromRgb
        { oldRgba
            | alpha = a
        }



-- LINKS


fakeLink : String -> Element msg
fakeLink name =
    Element.link
        [ Element.Font.color (Element.rgb 0 0 1)
        , Element.Font.underline
        ]
        { url = "#"
        , label = Element.text name
        }



-- INPUTS
-- dropdownSelector : List ( Element msg, msg ) -> Element msg
-- dropdownSelector itemsAndMsgs =
--     Element.column
--         [ Element.Border.color <| Element.rgba 0 0 0 0.2
--         , Element.Border.width 1
--         , Element.Border.rounded 5
--         , Element.Background.color white
--         , Element.padding 10
--         , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
--         , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
--         , Element.Border.shadow
--             { offset = ( 2, 2 )
--             , size = 0
--             , blur = 10
--             , color = Element.rgba 0 0 0 0.1
--             }
--         ]
--         (itemsAndMsgs
--             |> List.map
--                 (\( el, msg ) ->
--                     Element.el
--                         [ Element.paddingXY 0 5
--                         , onClickNoPropagation msg
--                         , Element.mouseOver [ Element.Background.color <| Element.rgb 0.8 0.8 1 ]
--                         ]
--                         el
--                 )
--         )
-- BUTTONS


button : DisplayProfile -> List (Attribute msg) -> ( Element.Color, Element.Color, Element.Color ) -> Element.Color -> List String -> msg -> Element msg
button dProfile attributes ( bgColor, bgHoverColor, bgPressedColor ) textColor lines msg =
    Element.column
        ([ Element.Border.rounded 4
         , Element.spacing (8 |> changeForMobile 5 dProfile)
         , Element.pointer
         , Element.Events.onClick msg
         , Element.paddingXY 25 17 |> changeForMobile (Element.padding 10) dProfile
         , Element.Font.color textColor
         , Element.Font.size (18 |> changeForMobile 16 dProfile)
         , Element.Font.semiBold
         , Element.Background.color bgColor
         , Element.mouseDown [ Element.Background.color bgPressedColor ]
         , Element.mouseOver [ Element.Background.color bgHoverColor ]
         , noSelectText
         ]
            ++ attributes
        )
        (List.map
            (Element.el [ Element.centerX, Element.centerY ] << Element.text)
            lines
        )



-- STYLE HELPERS


withIdAttribute : String -> Attribute msg
withIdAttribute s =
    Html.Attributes.id s
        |> Element.htmlAttribute


textWithoutTextCursor : String -> Element msg
textWithoutTextCursor s =
    Html.Styled.styled
        Html.Styled.span
        [ Css.hover [ Css.cursor Css.default ] ]
        []
        [ Html.Styled.text s ]
        |> Html.Styled.toUnstyled
        |> Element.html


onClickNoPropagation : msg -> Attribute msg
onClickNoPropagation msg =
    Html.Events.stopPropagationOn
        "click"
        (Json.Decode.succeed ( msg, True ))
        |> Element.htmlAttribute


roundBottomCorners : Int -> Attribute msg
roundBottomCorners r =
    Element.Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = r
        , bottomRight = r
        }


roundTopCorners : Int -> Attribute msg
roundTopCorners r =
    Element.Border.roundEach
        { topLeft = r
        , topRight = r
        , bottomLeft = 0
        , bottomRight = 0
        }


withHeader : String -> Element msg -> Element msg
withHeader headerString element =
    Element.column [ Element.spacing 10 ]
        [ Element.el
            [ Element.Font.size 17
            , Element.Font.semiBold
            ]
            (Element.text headerString)
        , element
        ]



-- SPECIAL CHARS


bulletPointString : String
bulletPointString =
    Char.fromCode 8226
        |> String.fromChar



-- ETC


modal : Element.Color -> Bool -> msg -> msg -> Element msg -> Element msg
modal overlayColor includeScrollbarY clickInsideMsg clickOutsideMsg el =
    Element.el
        ([ Element.behindContent <|
            Element.el
                [ Element.Background.color overlayColor
                , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
                , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
                , Element.htmlAttribute <| Html.Attributes.style "top" "0"
                , Element.htmlAttribute <| Html.Attributes.style "left" "0"
                , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
                , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                , Element.Events.onClick clickOutsideMsg
                ]
                Element.none
         , Element.width Element.fill
         , Element.height Element.fill
         , onClickNoPropagation clickInsideMsg
         ]
            ++ (if includeScrollbarY then
                    [ Element.scrollbarY ]

                else
                    []
               )
        )
        el



-- closeableModal : Bool -> List (Attribute msg) -> Element msg -> msg -> msg -> Bool -> Element msg
-- closeableModal isBlack extraAttributes innerEl clickInsideMsg closeMsg includeScrollbarY =
--     modal
--         (Element.rgba 0 0 0.3 0.6)
--         includeScrollbarY
--         clickInsideMsg
--         closeMsg
--     <|
--         Element.el
--             ([ Element.centerX
--              , Element.centerY
--              , Element.width (Element.fill |> Element.maximum 700)
--              , Element.Background.color white
--              , Element.Border.rounded 8
--              , Element.inFront <|
--                 Element.el
--                     [ Element.alignRight
--                     , Element.alignTop
--                     ]
--                     (closeButton isBlack closeMsg)
--              ]
--                 ++ extraAttributes
--             )
--             innerEl
-- closeableModalBlackX =
--     closeableModal True
-- closeableModalWhiteX =
--     closeableModal False
-- txProcessModal : List (Element msg) -> msg -> msg -> msg -> Element msg
-- txProcessModal textLines clickInsideMsg closeMsg clickOutsideMsg =
--     modal
--         (Element.rgba 0 0 0.3 0.6)
--         False
--         clickInsideMsg
--         clickOutsideMsg
--     <|
--         Element.column
--             [ Element.spacing 10
--             , Element.centerX
--             , Element.centerY
--             , Element.Background.color <| Element.rgba 0 0 0 0.5
--             , Element.Border.rounded 8
--             , Element.padding 20
--             , Element.inFront <|
--                 Element.el
--                     [ Element.alignRight
--                     , Element.alignTop
--                     ]
--                     (closeButton False closeMsg)
--             ]
--             (textLines
--                 |> List.map
--                     (\line ->
--                         Element.paragraph
--                             [ Element.centerX
--                             , Element.centerY
--                             , Element.Font.size 20
--                             , Element.Font.semiBold
--                             , Element.Font.color white
--                             , Element.Font.center
--                             ]
--                             [ line ]
--                     )
--             )


subtleShadow : Attribute msg
subtleShadow =
    Element.Border.shadow
        { offset = ( 0, 3 )
        , size = 0
        , blur = 20
        , color = Element.rgba255 0 0 0 0.04
        }


closeButton : Bool -> msg -> Element msg
closeButton isBlack msg =
    Element.el
        [ Element.padding 10
        , Element.Events.onClick msg
        , Element.pointer
        ]
        (Element.image [ Element.width <| Element.px 22 ]
            { src =
                if isBlack then
                    "img/remove-circle-black.svg"

                else
                    "img/remove-circle-white.svg"
            , description = "close"
            }
        )


elOnCircle : List (Attribute msg) -> Int -> Element.Color -> Element msg -> Element msg
elOnCircle attributes width color el =
    let
        circleElement =
            Collage.circle (toFloat width / 2)
                |> Collage.filled (Collage.uniform (elementColorToAvh4Color color))
                |> Collage.Render.svg
                |> Element.html
    in
    Element.el
        ([ Element.inFront <|
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
                el
         ]
            ++ attributes
        )
        circleElement


elementColorToAvh4Color : Element.Color -> Color
elementColorToAvh4Color c =
    Element.toRgb c
        |> (\rgba ->
                Color.rgba
                    rgba.red
                    rgba.green
                    rgba.blue
                    rgba.alpha
           )


scrollbarYEl : List (Attribute msg) -> Element msg -> Element msg
scrollbarYEl attrs body =
    Element.el [ Element.height Element.fill, Element.width Element.fill ] <|
        Element.el
            ([ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
             , Element.htmlAttribute <| Html.Attributes.style "top" "0"
             , Element.htmlAttribute <| Html.Attributes.style "right" "0"
             , Element.htmlAttribute <| Html.Attributes.style "bottom" "0"
             , Element.htmlAttribute <| Html.Attributes.style "left" "0"
             , Element.scrollbarY
             ]
                ++ attrs
            )
            body


thinGrayHRuler : Element msg
thinGrayHRuler =
    Element.el
        [ Element.height <| Element.px 1
        , Element.width Element.fill
        , Element.Background.color <| Element.rgba 0 0 0 0.2
        ]
        Element.none


moveToFront : Attribute msg
moveToFront =
    Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"


noSelectText : Attribute msg
noSelectText =
    Element.htmlAttribute <|
        Html.Attributes.style "user-select" "none"

visibility : Bool -> Attribute msg
visibility flag =
    Element.htmlAttribute <|
        Html.Attributes.style "visibility" <|
            if flag then
                "visible"
            else
                "hidden"