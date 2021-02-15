module View.Sidebar exposing (view)

import Element exposing (Element, centerX, centerY, column, el, fill, height, paddingXY, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, white)
import Misc
import Theme exposing (orange, softRed)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (cappedWidth, hover, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement, when)
import View.Img
import Wallet


view : Model -> Element Msg
view model =
    [ walletUXPane model.dProfile model.showAddressId model.demoPhaceSrc model.wallet
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
            |> viewTopics model.ethPrice
      ]
        |> column
            [ width fill
            , whiteGlowAttributeSmall
            , slightRound
            ]
        |> when (model.view /= ViewTopics)
    ]
        |> column
            [ cappedWidth 400
            , spacing 10
            , height fill
            ]


viewTopics : Float -> List ( String, TokenValue ) -> Element Msg
viewTopics ethPrice =
    List.map
        (\( topic, totalBurned ) ->
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
                        |> text
                        |> el [ width fill, Font.size 30 ]
                    , [ Element.image
                            [ height <| px 25
                            ]
                            { src = "./favicon.svg"
                            , description = "smokesignal logo"
                            }
                      , View.Img.dollar 25 softRed
                      , totalBurned
                            |> Misc.tokenToDollar ethPrice
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
        >> column [ width fill ]


walletUXPane :
    DisplayProfile
    -> Maybe PhaceIconId
    -> String
    -> Types.Wallet
    -> Element Msg
walletUXPane dProfile showAddressId demoPhaceSrc wallet =
    let
        phaceEl =
            case Wallet.userInfo wallet of
                Nothing ->
                    phaceElement
                        ( 80, 80 )
                        True
                        (Eth.Utils.unsafeToAddress demoPhaceSrc)
                        (showAddressId == Just DemoPhace)
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
                        (showAddressId == Just UserPhace)
                        (ShowOrHideAddress UserPhace)
                        |> el
                            [ Border.rounded 10
                            , Border.glow
                                (Element.rgba 0 0.5 1 0.4)
                                9
                            ]

        ( buttonText, maybeButtonAction, maybeExplainerText ) =
            case wallet of
                Types.NoneDetected ->
                    ( "Install Metamask"
                    , Just <| EH.NewTabLink "https://metamask.io/"
                    , Just "Then come back to try on some phaces!"
                    )

                Types.OnlyNetwork _ ->
                    ( "Connect Wallet"
                    , Just <| EH.Action ConnectToWeb3
                    , Just "Each address has a unique phace!"
                    )

                Types.Active userInfo ->
                    let
                        userHasNoEth =
                            userInfo.balance
                                |> Maybe.map TokenValue.isZero
                                |> Maybe.withDefault False
                    in
                    if userHasNoEth then
                        ( "Compose Post"
                        , Nothing
                        , Just "That address has no ETH! You need ETH to post on SmokeSignal."
                        )

                    else
                        ( "Compose Post"
                        , Just <| EH.Action <| ComposeOpen
                        , Nothing
                        )

        button =
            let
                attributes =
                    [ paddingXY 10 5
                    , width fill
                    ]
                        ++ (case maybeExplainerText of
                                Nothing ->
                                    [ Element.centerY
                                    ]

                                _ ->
                                    []
                           )
            in
            case maybeButtonAction of
                Just buttonAction ->
                    Theme.unscaryButton
                        dProfile
                        attributes
                        [ buttonText ]
                        buttonAction

                Nothing ->
                    Theme.disabledButton
                        dProfile
                        attributes
                        buttonText

        explainerParagraphOrNone =
            maybeExplainerText
                |> Maybe.map
                    (\text ->
                        Element.paragraph
                            [ Font.color EH.white
                            , Font.size 16
                            ]
                            [ Element.text text ]
                    )
                |> Maybe.withDefault Element.none
    in
    [ phaceEl
    , column
        [ width fill
        , spaceEvenly
        , height fill
        ]
        [ button
        , explainerParagraphOrNone
        ]
    ]
        |> row
            [ width fill
            , spacing 10
            ]
