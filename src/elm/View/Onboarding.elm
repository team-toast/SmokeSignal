module View.Onboarding exposing (view, viewOnboarding__)

import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Maybe.Extra exposing (unwrap)
import Theme exposing (orange)
import TokenValue
import Types exposing (..)
import View.Attrs exposing (hover, whiteGlowAttributeSmall)
import View.Common exposing (when, whenAttr, wrapModal)
import View.Img
import Wallet


view : Model -> Element Msg
view model =
    (if model.wallet == NoneDetected then
        viewOnboarding_ model

     else
        viewOnboarding model
    )
        |> (if model.dProfile == Mobile then
                identity

            else
                wrapModal OnboardingClose
           )


viewOnboarding_ : Model -> Element Msg
viewOnboarding_ model =
    let
        isMobile =
            model.dProfile == Mobile
    in
    [ [ text "To post or interact with ", el [ Font.bold ] (text "SmokeSignal"), text ", you'll need a crypto identity." ]
        |> paragraph [ Font.center, Font.size 22 ]
    , [ text "Install and setup "
      , Element.newTabLink
            [ Font.color Theme.orange, hover, Font.bold ]
            { url = "https://metamask.io/"
            , label = text "MetaMask"
            }
      , text ", then refresh."
      ]
        |> paragraph [ Font.center, Font.size 22 ]
    , Input.button
        [ Font.underline
        , View.Attrs.hover
        , View.Attrs.sansSerifFont
        , Element.alignRight
        ]
        { onPress = Just OnboardingClose
        , label = text "Back"
        }
    ]
        |> column
            [ padding 30
            , spacing 30
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , width fill
            , centerY
                |> View.Common.whenAttr (model.dProfile == Mobile)
            , height fill
                |> whenAttr isMobile
            , View.Attrs.style "z-index" "2000"
                |> whenAttr isMobile
            ]


viewOnboarding__ : Model -> Element Msg
viewOnboarding__ model =
    let
        isMobile =
            model.dProfile == Mobile

        step1 =
            not (model.wallet == Types.NoneDetected)

        step2 =
            Wallet.isActive model.wallet

        step3 =
            model.wallet
                |> Wallet.userInfo
                |> unwrap False (.chain >> (==) XDai)

        step4 =
            --model.wallet
            --|> Wallet.userInfo
            --|> unwrap False
            --(\userInfo ->
            --userInfo.chain
            --== XDai
            --&& (userInfo.balance |> TokenValue.isZero |> not)
            --)
            model.hasOnboarded

        showFaucet =
            model.wallet
                |> Wallet.userInfo
                |> unwrap False
                    (\userInfo ->
                        userInfo.chain == XDai && TokenValue.isZero userInfo.balance
                    )
    in
    [ [ text "Using xDai with SmokeSignal will result in much lower transaction fees than using Ethereum." ]
        |> paragraph [ Font.center, Font.size 22 ]
    , [ [ text "Switch to xDai chain"
        ]
            |> viewCheck
                step3
                model.chainSwitchInProgress
            |> (\elem ->
                    if step1 && step2 && not step3 then
                        Input.button [ hover |> (whenAttr <| not model.chainSwitchInProgress) ]
                            { onPress =
                                if model.chainSwitchInProgress then
                                    Nothing

                                else
                                    Just Types.XDaiImport
                            , label = elem
                            }

                    else if step3 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      , [ text "Add xDai funds"
        ]
            |> viewCheck
                step4
                model.faucetInProgress
            |> (\elem ->
                    if step1 && step2 && step3 && not step4 then
                        Input.button [ hover |> (whenAttr <| not model.faucetInProgress) ]
                            { onPress =
                                if model.faucetInProgress then
                                    Nothing

                                else
                                    Just SubmitFaucet
                            , label = elem
                            }

                    else if step4 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      , Input.button
            [ Background.color Theme.orange
            , padding 10
            , View.Attrs.roundBorder
            , hover
            , Font.color black
            ]
            { onPress = Just SubmitFaucet
            , label =
                if model.faucetInProgress then
                    View.Common.spinner 20 black
                        |> el [ centerX ]

                else
                    text "Request xDai from faucet"
            }
            |> when showFaucet
      , model.onboardMessage
            |> View.Common.whenJust
                (text
                    >> List.singleton
                    >> paragraph
                        [ Background.color white
                        , Element.alignRight
                        , View.Attrs.slightRound
                        , padding 10
                        , Font.color black
                        , Font.alignRight
                        ]
                )
      ]
        |> column [ spacing 20, width fill ]
    ]
        |> column
            [ padding 30
            , spacing 30
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , width fill
            , height fill
                |> whenAttr isMobile
            , Element.alignTop
            ]


viewOnboarding : Model -> Element Msg
viewOnboarding model =
    let
        isMobile =
            model.dProfile == Mobile

        step1 =
            not (model.wallet == Types.NoneDetected)

        step2 =
            Wallet.isActive model.wallet

        step3 =
            model.wallet
                |> Wallet.userInfo
                |> unwrap False (.chain >> (==) XDai)

        step4 =
            --model.wallet
            --|> Wallet.userInfo
            --|> unwrap False
            --(\userInfo ->
            --userInfo.chain
            --== XDai
            --&& (userInfo.balance |> TokenValue.isZero |> not)
            --)
            model.hasOnboarded
    in
    [ [ text "To post or interact with SmokeSignal, you'll need a crypto identity:" ]
        |> paragraph [ Font.center, Font.size 22 ]
    , [ [ text "Install and setup "
        , Element.newTabLink
            [ Font.color Theme.orange, hover, Font.bold ]
            { url = "https://metamask.io/"
            , label = text "MetaMask"
            }
        , text ", then refresh"
        ]
            |> viewCheck step1 False
      , [ text "Connect wallet"
        ]
            |> viewCheck step2 (model.wallet == Connecting)
            |> (\elem ->
                    if step1 && not step2 then
                        Input.button [ hover |> whenAttr (not <| model.wallet == Connecting) ]
                            { onPress =
                                if model.wallet == Connecting then
                                    Nothing

                                else
                                    Just Types.ConnectToWeb3
                            , label = elem
                            }

                    else if step2 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      , [ text "Enable xDai support"
        ]
            |> viewCheck
                step3
                model.chainSwitchInProgress
            |> (\elem ->
                    if step1 && step2 && not step3 then
                        Input.button [ hover |> (whenAttr <| not model.chainSwitchInProgress) ]
                            { onPress =
                                if model.chainSwitchInProgress then
                                    Nothing

                                else
                                    Just Types.XDaiImport
                            , label = elem
                            }

                    else if step3 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      , [ text "Get free xDai"
        ]
            |> viewCheck
                step4
                model.faucetInProgress
            |> (\elem ->
                    if step1 && step2 && step3 && not step4 then
                        Input.button [ hover |> (whenAttr <| not model.faucetInProgress) ]
                            { onPress =
                                if model.faucetInProgress then
                                    Nothing

                                else
                                    Just SubmitFaucet
                            , label = elem
                            }

                    else if step4 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      ]
        |> column [ spacing 20, width fill ]
    , if model.hasOnboarded then
        Input.button
            [ Background.color Theme.orange
            , padding 10
            , View.Attrs.roundBorder
            , hover
            , Font.color black
            , Element.alignRight
            ]
            { onPress = Just OnboardingClose
            , label = text "Continue"
            }

      else
        View.Common.cancel OnboardingClose
            |> el [ Element.alignRight ]
    ]
        |> column
            [ padding 30
            , spacing 30
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , width fill
            , centerY
                |> View.Common.whenAttr (model.dProfile == Mobile)
            , height fill
                |> whenAttr isMobile
            , View.Attrs.style "z-index" "2000"
                |> whenAttr isMobile
            ]


viewCheck : Bool -> Bool -> List (Element Msg) -> Element Msg
viewCheck tick inProg elems =
    [ (if tick then
        View.Img.tick 25 black

       else if inProg then
        View.Common.spinner 20 black

       else
        Element.none
      )
        |> el
            [ centerX
            , centerY
            ]
        |> el
            [ width <| px 30
            , height <| px 30
            , Background.color white
            , whiteGlowAttributeSmall

            --, hover
            ]
    , elems
        |> paragraph []
    ]
        |> row [ width fill, spacing 20 ]
