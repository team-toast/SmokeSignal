module View.Wallet exposing (view, viewMobileWalletSuggestion)

import Element exposing (Element, centerX, column, el, fill, padding, paragraph, px, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Theme
import TokenValue
import Types exposing (Model, Msg)
import View.Attrs exposing (hover, whiteGlowAttributeSmall)
import View.Common
import View.Sidebar
import Wallet


view : Model -> Element Msg
view model =
    if model.wallet == Types.NoneDetected then
        viewMobileWalletSuggestion

    else
        [ View.Sidebar.viewWallet model
        , model.wallet
            |> Wallet.userInfo
            |> View.Common.whenJust
                (\info ->
                    case info.chain of
                        Types.Eth ->
                            [ [ el [ Font.bold ] (text "Note:")
                              , text " Posting on SmokeSignal using Ethereum can result in very high gas fees. Using xDai is a cheaper alternative."
                              ]
                                |> paragraph []
                            , viewWalletsText
                            ]
                                |> column
                                    [ Background.color black
                                    , padding 10
                                    , spacing 10
                                    , whiteGlowAttributeSmall
                                    , Font.color white
                                    ]

                        Types.XDai ->
                            [ [ text "Your xDai wallet is currently empty."
                              ]
                                |> paragraph [ Font.center ]
                            , model.compose.message
                                |> View.Common.whenJust
                                    (text
                                        >> List.singleton
                                        >> paragraph
                                            [ Background.color white
                                            , Element.alignRight
                                            , View.Attrs.slightRound
                                            , padding 10
                                            , Font.color black
                                            ]
                                    )
                            , Input.button
                                [ Background.color Theme.green
                                , padding 10
                                , View.Attrs.roundBorder
                                , hover
                                , Font.color black
                                , width <| px 240
                                , centerX
                                ]
                                { onPress = Just Types.SubmitFaucet
                                , label =
                                    if info.xDaiStatus == Types.WaitingForApi || info.xDaiStatus == Types.WaitingForBalance then
                                        View.Common.spinner 20 black
                                            |> el [ centerX ]

                                    else
                                        [ text "Request xDai from faucet" ]
                                            |> paragraph [ Font.center ]
                                }
                            ]
                                |> column
                                    [ Background.color black
                                    , padding 20
                                    , spacing 20
                                    , whiteGlowAttributeSmall
                                    , Font.color white
                                    , width fill
                                    ]
                                |> View.Common.when (TokenValue.isZero info.balance)
                )
        ]
            |> column [ width fill, spacing 20 ]


viewMobileWalletSuggestion : Element msg
viewMobileWalletSuggestion =
    [ [ text "To post or interact with ", el [ Font.bold ] (text "SmokeSignal"), text ", you'll need a crypto identity." ]
        |> paragraph [ Font.center, Font.size 22 ]
    , [ text "On mobile you will need a wallet app with a DApp browser to accomplish this."
      ]
        |> paragraph [ Font.center, Font.size 22 ]
    , viewWalletsText
        |> el [ width fill, Font.center, Font.size 22 ]
    ]
        |> column
            [ padding 30
            , spacing 30
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , fill
                |> Element.minimum 240
                |> width
            ]


viewWalletsText : Element msg
viewWalletsText =
    [ text "We recommend using "
    , Element.newTabLink [ Font.bold, Font.color Theme.orange ]
        { url = "https://status.im/"
        , label = text "Status"
        }
    , text " or "
    , Element.newTabLink [ Font.bold, Font.color Theme.orange ]
        { url = "https://alphawallet.com/"
        , label = text "AlphaWallet"
        }
    , text " for easy xDai support."
    ]
        |> paragraph []
