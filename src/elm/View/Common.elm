module View.Common exposing (appStatusMessage, burn, cancel, chain, ellipsisText, horizontalRule, link, phaceElement, spinner, timingOrSpinner, topic, verticalRule, when, whenAttr, whenJust, wrapModal)

{-| A module for managing elm-ui 'Element' helper functions and reuseable components.
-}

import Element exposing (Attribute, Color, Element, centerX, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, white)
import Helpers.Time as TimeHelpers
import Html
import Html.Attributes
import Maybe.Extra exposing (unwrap)
import Misc
import Phace
import Theme
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, style)
import View.Img


topic : String -> Element msg
topic =
    (++) "#"
        >> text
        >> el [ Font.color Theme.orange ]


chain : Types.Chain -> Element msg
chain c =
    let
        txt =
            case c of
                Types.XDai ->
                    "xDai"

                Types.Eth ->
                    "Ethereum"

        img =
            case c of
                Types.XDai ->
                    View.Img.xDai 20

                Types.Eth ->
                    View.Img.eth 20 <| Element.rgb 0.5 0.5 1
    in
    [ img, text txt ]
        |> row
            [ spacing 10
            , View.Attrs.sansSerifFont
            ]


appStatusMessage : Element.Color -> String -> Element Msg
appStatusMessage color errStr =
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.paragraph
            [ Element.centerX
            , Element.centerY
            , Font.center
            , Font.italic
            , Font.color color
            , Font.size 36
            , Element.width (Element.fill |> Element.maximum 800)
            , Element.padding 40
            ]
            [ Element.text errStr ]


phaceElement : Int -> Address -> Bool -> Msg -> Element Msg
phaceElement size fromAddress showAddress onClick =
    let
        addressOutputEl () =
            -- delay processing because addressToChecksumString is expensive!
            Element.el
                [ Element.alignBottom

                --, if addressHangToRight then
                --Element.alignLeft
                --else
                --Element.alignRight
                , Background.color EH.white
                , Font.size 12
                , EH.moveToFront
                , Border.width 2
                , Border.color EH.black
                , Font.color black
                , padding 3
                , View.Attrs.typeFont

                --, EH.onClickNoPropagation noOpMsg
                ]
                (Element.text <| Eth.Utils.addressToChecksumString fromAddress)
    in
    Phace.fromEthAddress fromAddress size size
        |> Element.html
        |> el
            [ Border.rounded 5
            , Element.clip
            , Element.pointer
            , EH.onClickNoPropagation onClick

            -- , Border.width 1
            -- , Border.color Theme.blue
            ]
        |> el
            [ addressOutputEl ()
                |> Element.inFront
                |> whenAttr showAddress
            , hover
            ]


when : Bool -> Element msg -> Element msg
when b elem =
    if b then
        elem

    else
        Element.none


whenJust : (a -> Element msg) -> Maybe a -> Element msg
whenJust fn =
    Maybe.map fn >> Maybe.withDefault Element.none


whenAttr : Bool -> Attribute msg -> Attribute msg
whenAttr bool =
    if bool then
        identity

    else
        Element.below Element.none
            |> always


{-| Wraps an element with transparent clickable areas.
-}
wrapModal : msg -> Element msg -> Element msg
wrapModal msg elem =
    let
        btn =
            Input.button
                [ height fill
                , width fill
                , Background.color <| Element.rgba255 0 0 0 0.4
                ]
                { onPress = Just msg
                , label = Element.none
                }
    in
    [ btn
    , [ btn, elem, btn ]
        |> column [ width fill, height fill ]
    , btn
    ]
        |> row
            [ width fill
            , height fill
            , View.Attrs.style "z-index" "2000"
            ]


verticalRule : Color -> Element msg
verticalRule col =
    Element.none
        |> el [ width <| px 1, height fill, Background.color col ]


horizontalRule : Color -> Element msg
horizontalRule col =
    Element.none
        |> el [ width fill, height <| px 1, Background.color col ]


ellipsisText : Int -> String -> Element msg
ellipsisText n txt =
    Html.div
        [ Html.Attributes.style "overflow" "hidden"
        , Html.Attributes.style "text-overflow" "ellipsis"
        , Html.Attributes.style "white-space" "nowrap"
        , Html.Attributes.style "height" <| String.fromInt n ++ "px"
        , Html.Attributes.style "display" "table-cell"
        , Html.Attributes.title txt
        ]
        [ Html.text txt
        ]
        |> Element.html
        |> el
            [ width fill
            , style "width" "100%"
            , style "table-layout" "fixed"
            , style "display" "table"
            ]


cancel : msg -> Element msg
cancel msg =
    Input.button
        [ Font.underline
        , View.Attrs.hover
        , View.Attrs.sansSerifFont
        ]
        { onPress = Just msg
        , label = text "Cancel"
        }


spinner : Int -> Color -> Element msg
spinner size color =
    View.Img.spinner size color
        |> el [ View.Attrs.rotate ]


timingOrSpinner : Time.Posix -> Maybe Time.Posix -> Element Msg
timingOrSpinner now =
    unwrap
        (spinner 20 white)
        (\time ->
            TimeHelpers.sub now time
                |> TimeHelpers.roundToSingleUnit
                |> (\s -> s ++ " ago")
                |> text
                |> el [ View.Attrs.title (Misc.formatPosix time) ]
        )


link : String -> String -> Element msg
link url txt =
    Element.newTabLink
        [ hover, Font.color Theme.orange, Font.underline ]
        { url = url
        , label =
            [ text txt ]
                |> Element.paragraph []
        }


burn : TokenValue -> Element msg
burn amount =
    row
        []
        [ View.Img.dollar 25 Theme.softRed
        , row [ spacing 5 ]
            [ amount
                |> Misc.formatDollar
                |> text
                |> el [ Font.size 25, Font.bold, Font.color Theme.softRed ]
            , Element.image
                [ height <| px 25
                ]
                { src = "./favicon.svg"
                , description = "smokesignal logo"
                }
            ]
        ]
