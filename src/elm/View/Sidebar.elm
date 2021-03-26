module View.Sidebar exposing (view, viewWallet)

import Chain
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
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
    ]
        |> column
            [ cappedWidth 350
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
                , paddingXY 10 5
                , hover
                , View.Attrs.title topic
                ]
                { onPress = Just <| GotoView <| ViewTopic topic
                , label =
                    [ [ text topic ]
                        |> Element.paragraph [ width fill, Font.size 30 ]
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
                        |> row [ Element.alignTop ]
                    ]
                        |> row
                            [ width fill
                            ]
                }
        )
        >> column [ width fill ]


viewWallet : Model -> Element Msg
viewWallet model =
    let
        phaceEl =
            case Wallet.userInfo model.wallet of
                Nothing ->
                    phaceElement
                        90
                        (Eth.Utils.unsafeToAddress model.demoPhaceSrc)
                        (model.showAddressId == Just DemoPhace)
                        (ShowOrHideAddress DemoPhace)

                Just userInfo ->
                    phaceElement
                        90
                        userInfo.address
                        (model.showAddressId == Just UserPhace)
                        (ShowOrHideAddress UserPhace)

        ( buttonText, maybeButtonAction, maybeExplainerText ) =
            case model.wallet of
                Types.NoneDetected ->
                    ( "Get started"
                    , Just <| EH.Action ComposeOpen
                      --, Just "Each address has a unique phace!"
                    , Nothing
                    )

                Types.NetworkReady ->
                    ( "Connect Wallet"
                    , Just <| EH.Action ConnectToWeb3
                    , Just "Each address has a unique phace!"
                    )

                Types.Connecting ->
                    ( "Connecting"
                    , Nothing
                    , Just "Please complete the MetaMask connection process"
                    )

                Types.Active userInfo ->
                    let
                        userHasNoEth =
                            TokenValue.isZero userInfo.balance
                    in
                    if userHasNoEth then
                        ( "Compose Post"
                        , Just <| EH.Action <| ComposeOpen
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
            |> View.Common.whenJust
                (\userInfo ->
                    viewChain userInfo.chain
                )
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
    chain
        |> View.Common.viewChain
        |> el
            [ Element.padding 10
            , View.Attrs.roundBorder
            , Background.color <| Chain.getColor chain
            , Font.color white
            ]
