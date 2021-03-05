module View.Sidebar exposing (view, viewWallet)

import Chain
import Element exposing (Element, centerX, centerY, column, el, fill, height, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, white)
import Misc
import Theme exposing (orange, softRed)
import TokenValue
import Types exposing (..)
import View.Attrs exposing (cappedWidth, hover, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement, when)
import View.Img
import Wallet


view : Model -> Element Msg
view model =
    [ viewWallet model
    , [ Input.button [ height <| px 30, width fill, hover ]
            { onPress = Just <| GotoView ViewTopics
            , label =
                text "Topics"
                    |> el [ centerX, centerY ]
                    |> el
                        [ width fill
                        , height <| px 30
                        , Background.color Theme.orange
                        ]
            }
      , model.topics
            |> Misc.sortTopics
            |> viewTopics
      ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            , slightRound
            ]
        |> when (model.view /= ViewTopics)
    , Element.newTabLink
        [ Font.color white
        , View.Attrs.sansSerifFont
        , Font.bold
        , centerX
        , hover
        ]
        { url = model.alphaUrl
        , label =
            [ text "SmokeSignal Alpha", View.Img.link 20 white ]
                |> row [ spacing 10 ]
        }
    ]
        |> column
            [ cappedWidth 400
            , spacing 10
            , height fill
            ]


viewTopics : List ( String, Count ) -> Element Msg
viewTopics =
    List.map
        (\( topic, count ) ->
            Input.button
                [ width fill
                , Background.color black
                , Font.color white
                , paddingXY 15 5
                , hover
                ]
                { onPress = Just <| GotoView <| ViewTopic topic
                , label =
                    [ topic
                        |> View.Common.ellipsisText 30
                        |> el [ width fill, Font.size 30 ]
                    , [ Element.image
                            [ height <| px 25
                            ]
                            { src = "./favicon.svg"
                            , description = "smokesignal logo"
                            }
                      , View.Img.dollar 25 softRed
                      , count.total
                            |> Misc.formatDollar
                            |> text
                            |> el [ Font.size 25, Font.bold, Font.color softRed ]
                      ]
                        |> row []
                    ]
                        |> row
                            [ width fill
                            ]
                }
        )
        >> column [ width <| px 400 ]


viewWallet : Model -> Element Msg
viewWallet model =
    let
        phaceEl =
            case Wallet.userInfo model.wallet of
                Nothing ->
                    phaceElement
                        ( 80, 80 )
                        True
                        (Eth.Utils.unsafeToAddress model.demoPhaceSrc)
                        (model.showAddressId == Just DemoPhace)
                        (ShowOrHideAddress DemoPhace)
                        |> el
                            [ Border.rounded 10
                            , Border.glow
                                (Element.rgba 1 0 1 0.3)
                                9
                            ]

                Just userInfo ->
                    phaceElement
                        ( 100, 100 )
                        True
                        userInfo.address
                        (model.showAddressId == Just UserPhace)
                        (ShowOrHideAddress UserPhace)
                        |> el
                            [ Border.rounded 10
                            , Border.glow
                                (Element.rgba 0 0.5 1 0.4)
                                9
                            ]

        ( buttonText, maybeButtonAction, maybeExplainerText ) =
            case model.wallet of
                Types.NoneDetected ->
                    ( "Install Metamask"
                    , Just <| EH.NewTabLink "https://metamask.io/"
                    , Just "Then come back to try on some phaces!"
                    )

                Types.NetworkReady ->
                    ( "Connect Wallet"
                    , Just <| EH.Action ConnectToWeb3
                    , Just "Each address has a unique phace!"
                    )

                Types.Connecting ->
                    ( "Connecting"
                    , Nothing
                    , Nothing
                    )

                Types.Active userInfo ->
                    let
                        userHasNoEth =
                            TokenValue.isZero userInfo.balance
                    in
                    if userHasNoEth then
                        ( "Compose Post"
                        , Nothing
                        , "That address has no "
                            ++ Chain.getName userInfo.chain
                            ++ "! You will need to transfer some to post on SmokeSignal."
                            |> Just
                        )

                    else
                        ( "Compose Post"
                        , Just <| EH.Action <| ComposeOpen
                        , Nothing
                        )

        button =
            case maybeButtonAction of
                Just buttonAction ->
                    Theme.unscaryButton
                        model.dProfile
                        [ paddingXY 10 5
                        , width fill
                        ]
                        [ buttonText ]
                        buttonAction

                Nothing ->
                    Theme.disabledButton
                        model.dProfile
                        [ paddingXY 10 5
                        , width fill
                        ]
                        buttonText

        explainerParagraphOrNone =
            maybeExplainerText
                |> Maybe.map
                    (\text ->
                        Element.paragraph
                            [ Font.color EH.white
                            , Font.size 17
                            , View.Attrs.sansSerifFont
                            ]
                            [ Element.text text ]
                    )
                |> Maybe.withDefault Element.none
    in
    [ phaceEl
        |> el [ Element.alignTop ]
    , [ [ button
        , model.wallet
            |> Wallet.userInfo
            |> View.Common.whenJust (.chain >> viewChain)
        ]
            |> column [ spacing 10, width fill ]
      , explainerParagraphOrNone
      ]
        |> column
            [ width fill
            , spacing 10
            , height fill
            ]
    ]
        |> row
            [ width fill
            , spacing 10
            ]


viewChain : Types.Chain -> Element msg
viewChain chain =
    let
        col =
            case chain of
                Types.XDai ->
                    softRed

                Types.Eth ->
                    orange
    in
    chain
        |> View.Common.viewChain
        |> el
            [ Element.padding 10
            , View.Attrs.roundBorder
            , Background.color col
            ]
