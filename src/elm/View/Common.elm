module View.Common exposing (appStatusMessage, cancel, ellipsisText, phaceElement, spinner, verticalRule, viewChain, when, whenAttr, whenJust, wrapModal)

{-| A module for managing elm-ui 'Element' helper functions and reuseable components.
-}

import Element exposing (Attribute, Color, Element, column, el, fill, height, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black)
import Html
import Html.Attributes
import Phace
import Types exposing (..)
import View.Attrs exposing (style)
import View.Img


viewChain : Types.Chain -> Element msg
viewChain c =
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
                    View.Img.xDai

                Types.Eth ->
                    View.Img.eth
    in
    [ img 20, text txt ]
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


phaceElement : ( Int, Int ) -> Bool -> Address -> Bool -> Msg -> Element Msg
phaceElement ( width, height ) addressHangToRight fromAddress showAddress onClick =
    let
        addressOutputEl () =
            -- delay processing because addressToChecksumString is expensive!
            Element.el
                [ Element.alignBottom
                , if addressHangToRight then
                    Element.alignLeft

                  else
                    Element.alignRight
                , Background.color EH.white
                , Font.size 12
                , EH.moveToFront
                , Border.width 2
                , Border.color EH.black

                --, EH.onClickNoPropagation noOpMsg
                ]
                (Element.text <| Eth.Utils.addressToChecksumString fromAddress)
    in
    Element.el
        (if showAddress then
            [ Element.inFront <| addressOutputEl ()
            , Element.alignTop
            ]

         else
            [ Element.alignTop ]
        )
    <|
        Element.el
            [ Border.rounded 5
            , Element.clip
            , Element.pointer
            , EH.onClickNoPropagation onClick

            -- , Border.width 1
            -- , Border.color Theme.blue
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress width height)


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
