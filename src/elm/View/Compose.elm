module View.Compose exposing (view)

import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, responsiveVal, white)
import Maybe.Extra exposing (unwrap)
import Misc
import Theme exposing (orange, theme)
import Types exposing (..)
import View.Attrs exposing (hover, sansSerifFont, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (wrapModal)
import View.Img
import View.Markdown
import Wallet


view : Model -> Element Msg
view model =
    model.wallet
        |> Wallet.userInfo
        |> unwrap
            ([ text "Please connect your Metamask wallet." ]
                |> Element.paragraph [ Font.center, centerY ]
                |> el
                    [ padding 10
                    , whiteGlowAttributeSmall
                    , Background.color black
                    , Font.color white
                    , height <| px 250
                    , width fill
                    ]
            )
            (viewBox model)
        |> wrapModal ComposeClose


viewBox : Model -> UserInfo -> Element Msg
viewBox model userInfo =
    let
        validTopic =
            model.topicInput
                |> Misc.validateTopic
                |> (/=) Nothing

        submitEnabled =
            not (String.isEmpty model.compose.body)
                && not (String.isEmpty model.compose.dollar)
                && validTopic
    in
    [ "Comment"
        |> text
        |> el [ sansSerifFont, Font.color white, centerX ]
        |> el
            [ View.Attrs.sansSerifFont
            , padding 10
            , slightRound
            , Background.color Theme.orange
            , Font.bold
            , Font.color white
            , Font.size 20
            , width fill
            ]
    , [ [ [ Input.text
                [ width fill
                , View.Attrs.whiteGlowAttributeSmall
                ]
                { onChange = ComposeTitleChange
                , label = Input.labelHidden ""
                , placeholder =
                    "Title"
                        |> text
                        |> Input.placeholder []
                        |> Just
                , text = model.compose.title
                }
          , case model.compose.context of
                TopLevel _ ->
                    Input.text
                        [ Background.color white
                        , width <| px 250
                        , Border.roundEach
                            { bottomLeft = 0
                            , topLeft = 0
                            , bottomRight = 5
                            , topRight = 5
                            }
                        , spacing 0
                        ]
                        { onChange = TopicInputChange
                        , label =
                            "Topic"
                                |> text
                                |> el [ centerY ]
                                |> Input.labelLeft
                                    [ Background.color orange
                                    , Border.roundEach
                                        { bottomLeft = 5
                                        , topLeft = 5
                                        , bottomRight = 0
                                        , topRight = 0
                                        }
                                    , height fill
                                    , Element.paddingXY 10 0
                                    , sansSerifFont
                                    ]
                        , placeholder = Nothing
                        , text = model.topicInput
                        }

                Reply _ ->
                    [ View.Img.replyArrow 25 orange
                    , "Reply"
                        |> text
                        |> el [ Font.color orange ]
                    ]
                        |> row [ spacing 10 ]
          ]
            |> column [ width fill, height fill, spacing 10 ]

        --, [ text "🔥"
        --|> el [ Font.size 30 ]
        --, text "BURN"
        --|> el [ Font.size 15, centerX ]
        --]
        --|> column
        --[ spacing 10
        --, Font.color orange
        --, Font.bold
        --]
        , [ View.Common.viewChain userInfo.chain
                |> el
                    [ Background.color white
                    , View.Attrs.roundBorder
                    , padding 5
                    , Element.alignRight
                    ]
          , [ View.Img.dollar 30 white
            , Input.text
                [ View.Attrs.whiteGlowAttributeSmall
                , Background.color white
                , width <| px 250
                ]
                { onChange = ComposeDollarChange
                , label = Input.labelHidden ""
                , placeholder =
                    "00.00"
                        |> text
                        |> Input.placeholder []
                        |> Just
                , text = model.compose.dollar
                }
            ]
                |> row [ spacing 5 ]
          , [ Input.checkbox
                [ width <| px 30
                , height <| px 30
                , Background.color white
                , whiteGlowAttributeSmall
                , hover
                ]
                { onChange = Types.DonationCheckboxSet
                , icon =
                    \checked ->
                        "✔️"
                            |> text
                            |> el
                                [ centerX
                                , centerY
                                , Font.size 25
                                ]
                            |> View.Common.when checked
                , checked = model.compose.donate
                , label = Input.labelHidden "Donate an extra 1% to Foundry"
                }
            , [ text "Donate an extra 1% to "
              , Element.newTabLink
                    [ Font.color theme.linkTextColor, hover ]
                    { url = "https://foundrydao.com/"
                    , label = text "Foundry"
                    }
              , text " so we can build more cool stuff!"
              ]
                |> Element.paragraph [ spacing 5, Font.color white ]
            ]
                |> row
                    [ Font.size (responsiveVal model.dProfile 14 10)
                    , spacing 10
                    ]
          ]
            |> column [ height fill, spacing 10 ]

        --, [ text "💎"
        --|> el [ Font.size 30 ]
        --, text "ETH"
        --|> el [ Font.size 15, centerX ]
        --]
        --|> column
        --[ spacing 10
        --, Font.color orange
        --, Font.bold
        --]
        --, phaceElement
        --( 75, 75 )
        --False
        --userInfo.address
        --False
        --ClickHappened
        --|> el [ centerY ]
        ]
            |> row [ width fill, spacing 20, sansSerifFont ]
      , [ Input.multiline
            [ width <| px 500
            , height <| px 500
            , View.Attrs.whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , Element.scrollbarY

            --, View.Attrs.style "min-height" "auto"
            ]
            { onChange = Types.ComposeBodyChange
            , label = Input.labelHidden ""
            , placeholder =
                "What do you want to say?"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = model.compose.body
            , spellcheck = True
            }
        , model.compose.body
            |> View.Markdown.renderString model.dProfile
            |> el
                [ width <| px 500

                --, height fill
                , Element.scrollbarY
                , height <| px 500

                --, View.Attrs.style "min-height" "auto"
                , whiteGlowAttributeSmall
                , Font.color white
                , padding 10
                ]
        ]
            |> row
                [ height fill
                , width fill
                , spacing 30
                , sansSerifFont
                ]
      , Input.button
            [ padding 10
            , Background.color orange
            , Element.alignRight
            , View.Attrs.roundBorder
            , if submitEnabled then
                hover

              else
                View.Attrs.style "cursor" "not-allowed"
            , sansSerifFont
            ]
            { onPress =
                if submitEnabled then
                    Just SubmitDraft

                else
                    Nothing
            , label = text "Comment"
            }
      ]
        |> column
            [ height fill
            , width fill
            , spacing 20
            , padding 20
            ]
    ]
        |> column
            [ width <| px 1000

            --, height <| px 700
            , Background.color black
            , whiteGlowAttributeSmall
            ]
