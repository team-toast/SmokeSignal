module View.About exposing (view)

import Element exposing (Element, centerX, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Maybe.Extra
import Theme
import TokenValue
import Types exposing (Model, Msg)
import View.Attrs exposing (cappedWidth, hover, whiteGlowAttribute)
import View.Common exposing (viewLink, when)
import Wallet


view : Model -> Element Msg
view model =
    let
        isMobile =
            model.dProfile == Helpers.Element.Mobile

        isXDai =
            model.wallet
                |> Wallet.userInfo
                |> Maybe.Extra.unwrap False (.chain >> (==) Types.XDai)

        walletActive =
            model.wallet
                |> Wallet.isActive

        balanceEmpty =
            model.wallet
                |> Wallet.userInfo
                |> Maybe.Extra.unwrap False
                    (.balance >> TokenValue.isZero)
    in
    [ Element.image
        [ width fill
        , Background.color black
        , whiteGlowAttribute
        , centerX
        ]
        { src = "./img/banner.png"
        , description = "Never be silenced"
        }
        |> when (not isMobile)
    , [ [ viewHeader "What is SmokeSignal?"
        , viewPara "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat."
        , viewPara "All SmokeSignal posts are permanent and impossible for anyone to delete."
        , viewPara "All SmokeSignal authors are pseudonymous by default and cannot be deplatformed."
        , [ text "All you need to post is a web3 wallet like "
          , viewLink "https://metamask.io/" "Metamask"
          , text " and "
          , viewLink "https://www.google.com/search?q=how+to+buy+ether"
                "ETH to burn"
          , text "."
          ]
            |> paragraph []
        , viewPara "The more ETH you burn, the more attention the post will get."
        ]
            |> column [ spacing 10 ]
      , [ viewHeader "What tokens are supported?"
        , [ text "SmokeSignal currently supports "
          , viewLink "https://ethereum.org/" "Ethereum"
          , text " and "
          , viewLink "https://www.xdaichain.com/" "xDai"
          , text "."
          ]
            |> paragraph []
        ]
            |> column [ width fill, spacing 10 ]
      , [ viewHeader "What is xDai?"
        , [ text "The xDai chain is a stable payments blockchain designed for fast and inexpensive transactions."
          ]
            |> paragraph []
        , [ Input.button
                [ padding 10
                , Font.color black
                , hover
                , Background.color Theme.orange
                , View.Attrs.roundBorder
                , View.Attrs.sansSerifFont
                ]
                { onPress = Just Types.XDaiImport
                , label = text "Switch to xDai"
                }
                |> when (not isXDai && walletActive)
          , Input.button
                [ padding 10
                , Font.color black
                , hover
                , Background.color Theme.orange
                , View.Attrs.roundBorder
                , View.Attrs.sansSerifFont
                ]
                { onPress =
                    if model.faucetInProgress then
                        Nothing

                    else
                        Just Types.SubmitFaucet
                , label =
                    if model.faucetInProgress then
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        text "Get free xDai"
                }
                |> when (isXDai && balanceEmpty)
          ]
            |> row [ spacing 10 ]
        ]
            |> column [ spacing 20 ]
      , Element.newTabLink [ hover, centerX ]
            { url = "https://github.com/team-toast/SmokeSignal"
            , label =
                [ Element.image [ height <| px 32 ]
                    { src = "./img/github.png", description = "" }
                , text "Source code"
                    |> el [ Font.underline ]
                ]
                    |> row [ spacing 10 ]
            }
      , [ text "Posts to previous versions of SmokeSignal can be found at "
        , Element.newTabLink
            [ Font.bold
            , hover
            , Font.underline
            ]
            { url = model.alphaUrl
            , label = text "SmokeSignal Alpha"
            }
        , text "."
        ]
            |> paragraph
                [ View.Attrs.sansSerifFont
                , Font.center
                ]
      ]
        |> column
            [ height fill
            , width fill
            , Background.color black
            , padding 20
            , spacing 20
            , Font.color white
            ]
    ]
        |> column
            [ height fill
            , cappedWidth 750
            , spacing 20
            , centerX
            ]


viewHeader : String -> Element msg
viewHeader =
    text
        >> List.singleton
        >> paragraph [ Font.size 30, Font.bold ]


viewPara : String -> Element msg
viewPara =
    text
        >> List.singleton
        >> paragraph []
