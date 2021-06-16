module View.Sidebar exposing (view, viewWallet)

import Chain
import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Eth.Utils
import Maybe.Extra exposing (unwrap)
import Misc
import Theme exposing (black, green, orange, white)
import Types exposing (..)
import View.Attrs exposing (cappedWidth, hover, roundBorder, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement, when, whenAttr, whenJust)
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
                , padding 5
                , hover
                , View.Attrs.title topic
                ]
                { onPress = Just <| GotoView <| ViewTopic topic
                , label =
                    row
                        [ width fill
                        , Font.size 26
                        ]
                        [ "#"
                            ++ topic
                            |> View.Common.ellipsisText 30
                            |> el [ Font.color Theme.orange, width fill ]
                        , View.Common.burn count.total
                        ]
                }
        )
        >> column [ width fill ]


viewWallet : Model -> Element Msg
viewWallet model =
    let
        phaceEl =
            model.wallet
                |> Wallet.userInfo
                |> unwrap
                    (phaceElement
                        90
                        (Eth.Utils.unsafeToAddress model.demoPhaceSrc)
                        (model.showAddressId == Just DemoPhace)
                        (ShowOrHideAddress DemoPhace)
                    )
                    (\userInfo ->
                        phaceElement
                            90
                            userInfo.address
                            (model.showAddressId == Just UserPhace)
                            (ShowOrHideAddress UserPhace)
                    )

        ( button, maybeExplainerText ) =
            case model.wallet of
                Types.NoneDetected ->
                    ( viewButton green "Get started" (Just OpenModal)
                    , Just "Each address has a unique phace!"
                    )

                Types.NetworkReady ->
                    ( viewButton green "Connect Wallet" (Just ConnectToWeb3)
                    , Just "Each address has a unique phace!"
                    )

                Types.Connecting ->
                    ( viewButton Theme.darkGray "Connecting" Nothing
                    , Just "Please complete the MetaMask connection process"
                    )

                Types.Active _ ->
                    ( viewButton green "Compose Post" (Just <| GotoView ViewCompose)
                    , Nothing
                    )
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
      , maybeExplainerText
            |> whenJust
                (\text ->
                    [ Element.text text ]
                        |> paragraph
                            [ Font.color white
                            , Font.size 17
                            , View.Attrs.sansSerifFont
                            ]
                )
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


viewButton : Color -> String -> Maybe msg -> Element msg
viewButton color txt msg =
    Input.button
        [ height <| px 30
        , width fill
        , hover
            |> whenAttr (msg /= Nothing)
        , Background.color color
        , Font.color white
        , roundBorder
        ]
        { onPress = msg
        , label =
            text txt
                |> el [ centerX, centerY ]
        }


viewChain : Types.Chain -> Element msg
viewChain chain =
    chain
        |> View.Common.chain
        |> el
            [ Element.padding 10
            , View.Attrs.roundBorder
            , Background.color <| Chain.getColor chain
            , Font.color white
            ]
