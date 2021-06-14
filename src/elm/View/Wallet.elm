module View.Wallet exposing (view, viewMobileWalletSuggestion)

import Element exposing (Element, column, el, fill, padding, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Theme exposing (black, white)
import Types exposing (Model, Msg)
import View.Attrs exposing (hover, whiteGlowAttributeSmall)
import View.Common
import View.Img
import View.Sidebar
import Wallet


view : Model -> Element Msg
view model =
    if model.wallet == Types.NoneDetected then
        [ viewMobileWalletSuggestion
        , viewWalletConnectButton
        ]
            |> column [ width fill, spacing 20 ]

    else
        [ View.Sidebar.viewWallet model
        , model.wallet
            |> Wallet.userInfo
            |> View.Common.whenJust
                (View.Common.viewInstructions
                    model.chainSwitchInProgress
                    model.dProfile
                )
        , viewWalletConnectButton
            |> View.Common.when (not <| Wallet.isActive model.wallet)
        ]
            |> column [ width fill, spacing 20 ]


viewWalletConnectButton : Element Msg
viewWalletConnectButton =
    Input.button
        [ Element.centerX
        , padding 10
        , View.Attrs.roundBorder
        , Background.color Theme.white
        , hover
        ]
        { onPress = Just Types.WalletConnectStart
        , label =
            [ View.Img.walletConnect 20
            , text "WalletConnect"
            ]
                |> row [ spacing 10 ]
        }


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
