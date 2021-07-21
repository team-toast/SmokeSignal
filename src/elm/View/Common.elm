module View.Common exposing (burn, cancel, chain, ellipsisText, horizontalRule, link, permapostUrl, phaceElement, scrollbarHack, spinner, timingOrSpinner, topic, verticalRule, viewInstructions, when, whenAttr, whenJust, wrapModal)

{-| A module for managing elm-ui 'Element' helper functions and reuseable components.
-}

import Element exposing (Attribute, Color, Element, centerX, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Types exposing (Address, TxHash)
import Eth.Utils
import Helpers.Time as TimeHelpers
import Html
import Html.Attributes
import Maybe.Extra exposing (unwrap)
import Misc
import Phace
import Theme exposing (black, white)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, style)
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


phaceElement : Int -> Address -> Bool -> Msg -> Element Msg
phaceElement size fromAddress showAddress onClick =
    let
        addressOutputEl () =
            -- delay processing because addressToChecksumString is expensive!
            Eth.Utils.addressToChecksumString fromAddress
                |> text
                |> List.singleton
                |> paragraph
                    [ --, if addressHangToRight then
                      --Element.alignLeft
                      --else
                      --Element.alignRight
                      Background.color white
                    , Font.size 12
                    , style "word-break" "break-word"
                    , width fill
                    , Border.width 2
                    , Border.color black
                    , Font.color black
                    , padding 3
                    , View.Attrs.typeFont

                    --, View.Attrs.moveToFront
                    --, EH.onClickNoPropagation noOpMsg
                    ]
    in
    Input.button
        [ (if showAddress then
            addressOutputEl ()

           else
            Element.none
          )
            |> Element.inFront
        , hover
        ]
        { onPress = Just onClick
        , label =
            Phace.fromEthAddress fromAddress size size
                |> Element.html
                |> el
                    [ Border.rounded 5
                    , Element.clip
                    ]
        }


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
            , View.Img.icon 25
            ]
        ]


viewInstructions : Bool -> DisplayProfile -> UserInfo -> Element Msg
viewInstructions chainSwitchInProgress dProfile userInfo =
    (case userInfo.chain of
        Eth ->
            [ [ el [ Font.bold ] (text "Note:")
              , text " Posting on SmokeSignal using Ethereum can result in very high gas fees. Using xDai is a cheaper alternative."
              ]
                |> paragraph [ Font.color black ]
            , Input.button
                [ Background.color Theme.green
                , padding 10
                , View.Attrs.roundBorder
                , hover
                , width <| px 180
                , Element.alignRight
                ]
                { onPress = Just XDaiImport
                , label =
                    if chainSwitchInProgress then
                        spinner 20 black
                            |> el [ centerX ]

                    else
                        text "Switch to xDai"
                            |> el [ centerX ]
                }
                |> when (userInfo.provider == Types.MetaMask)
            ]
                |> (if dProfile == Mobile then
                        column

                    else
                        row
                   )
                    [ width fill
                    , spacing 10
                    ]
                |> Just

        XDai ->
            if userInfo.balance |> unwrap False TokenValue.isZero then
                viewFaucet dProfile userInfo.faucetStatus
                    |> Just

            else
                Nothing
    )
        |> whenJust
            (el
                [ padding 10
                , Background.color white
                , roundBorder
                , width fill
                , Font.color black
                ]
            )


viewFaucet : DisplayProfile -> FaucetUX -> Element Msg
viewFaucet dProfile faucetStatus =
    case faucetStatus of
        FaucetStatus status ->
            [ [ [ el [ Font.bold ] (text "Note:")
                , text " Your xDai wallet is currently empty."
                ]
                    |> paragraph []
              , Input.button
                    [ Background.color Theme.green
                    , padding 10
                    , View.Attrs.roundBorder
                    , hover
                    , width <| px 240
                    ]
                    { onPress =
                        if status == Types.RequestInProgress then
                            Nothing

                        else
                            Just SubmitFaucet
                    , label =
                        if status == Types.RequestInProgress then
                            spinner 20 black
                                |> el [ centerX ]

                        else
                            [ text "Request xDai from faucet" ]
                                |> paragraph [ Font.center ]
                    }
              ]
                |> (if dProfile == Mobile then
                        column

                    else
                        row
                   )
                    [ width fill
                    , spacing 10
                    ]
            , case status of
                Types.RequestReady ->
                    Element.none

                Types.RequestInProgress ->
                    Element.none

                Types.RequestError message ->
                    [ text message ]
                        |> paragraph []
            ]
                |> column [ width fill, spacing 10 ]

        FaucetSuccess ->
            [ text "Your faucet request was successful. Please check your wallet for an updated balance."
            ]
                |> paragraph []


{-| Element.scrollbarY sometimes needs to be paired with
a column to work correctly.
-}
scrollbarHack : Element msg -> Element msg
scrollbarHack =
    List.singleton
        >> column [ width fill, height fill ]


permapostUrl : Chain -> TxHash -> String
permapostUrl chain_ hash =
    let
        permaposturl =
            "https://app.permapost.io/view?blockchain="
    in
    case chain_ of
        Eth ->
            permaposturl
                ++ "eth&tx="
                ++ Eth.Utils.txHashToString hash

        XDai ->
            permaposturl
                ++ "xdai&tx="
                ++ Eth.Utils.txHashToString hash
