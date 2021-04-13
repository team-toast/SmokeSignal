module View.Compose exposing (view, viewInstructions)

import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Html.Attributes
import Misc
import Theme exposing (orange)
import TokenValue
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (when, whenAttr, whenJust)
import View.Img
import View.Markdown


view : Model -> UserInfo -> Element Msg
view model userInfo =
    let
        isMobile =
            model.dProfile == Helpers.Element.Mobile

        validTopic =
            case model.compose.context of
                Reply _ ->
                    True

                TopLevel _ ->
                    model.topicInput
                        |> Misc.validateTopic
                        |> (/=) Nothing

        submitEnabled =
            not (String.isEmpty model.compose.body)
                && validTopic

        whitespace =
            if isMobile then
                10

            else
                20

        inputIsNonzero =
            model.compose.dollar
                |> String.toFloat
                |> Maybe.map (\f -> f /= 0)
                |> Maybe.withDefault False
    in
    [ "Compose"
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
    , [ [ viewInstructions model.chainSwitchInProgress model.dProfile userInfo
            |> when (not isMobile)
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
                        , Font.alignRight
                        ]
                )
        , Input.text
            [ width fill
            , View.Attrs.whiteGlowAttributeSmall
            ]
            { onChange = ComposeTitleChange
            , label = Input.labelHidden ""
            , placeholder =
                "Title (Optional)"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = model.compose.title
            }
        , [ viewBurnAmountUX model.compose.dollar
          , viewComposeContext model.compose.context model.topicInput
                |> el [ Element.alignRight ]
          , View.Common.chain userInfo.chain
                |> el
                    [ Background.color white
                    , View.Attrs.roundBorder
                    , padding 5
                    , Element.alignRight
                    ]
                |> when (not isMobile)
          ]
            |> (if isMobile then
                    column [ width fill, spacing 10 ]

                else
                    row [ width fill, spacing 10 ]
               )
        ]
            |> column [ width fill, spacing 10, sansSerifFont ]
      , viewMarkdown model
      , model.compose.error
            |> whenJust
                (text
                    >> List.singleton
                    >> paragraph
                        [ Background.color white
                        , Element.alignRight
                        , slightRound
                        , padding 10
                        , Font.color black
                        , Font.alignRight
                        ]
                )
      , [ [ Input.checkbox
                [ width <| px 30
                , height <| px 30
                , Background.color white
                , whiteGlowAttributeSmall
                , hover
                ]
                { onChange = Types.PreviewSet
                , icon =
                    \checked ->
                        View.Img.tick 20 black
                            |> el
                                [ centerX
                                , centerY
                                ]
                            |> View.Common.when checked
                , checked = model.compose.preview
                , label = Input.labelHidden "Preview"
                }
          , "Preview"
                |> text
                |> el [ Font.color white ]
          ]
            |> row [ spacing 5 ]
            |> when isMobile
        , [ View.Common.cancel ComposeClose
                |> el [ Font.color white ]
          , Input.button
                [ padding 10
                , Background.color orange
                , Element.alignRight
                , View.Attrs.roundBorder
                , if submitEnabled then
                    hover

                  else
                    View.Attrs.notAllowed
                , sansSerifFont
                ]
                { onPress =
                    if submitEnabled then
                        Just SubmitDraft

                    else
                        Nothing
                , label =
                    if model.compose.inProgress then
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        text "Submit"
                }
          ]
            |> row [ Element.alignRight, spacing 20 ]
        ]
            |> row [ width fill ]
      ]
        |> column
            [ height fill
            , width fill
            , spacing whitespace
            , padding whitespace
            ]
    ]
        |> column
            [ if isMobile then
                width fill

              else
                width <| px 1000
            , height fill
            , Background.color black
            , whiteGlowAttributeSmall
                |> whenAttr (not isMobile)
            , View.Attrs.style "z-index" "2000"
            , sansSerifFont
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
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        text "Switch to xDai"
                            |> el [ centerX ]
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
                |> Just

        XDai ->
            if TokenValue.isZero userInfo.balance then
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
                            View.Common.spinner 20 black
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


viewComposeContext : Context -> String -> Element Msg
viewComposeContext context topicInput =
    case context of
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
                , Element.Events.onLoseFocus SanitizeTopic
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
                , text = topicInput
                }

        Reply _ ->
            [ View.Img.replyArrow 25 orange
            , "Reply"
                |> text
                |> el [ Font.color orange ]
            ]
                |> row [ spacing 10 ]


viewBurnAmountUX : String -> Element Msg
viewBurnAmountUX amountInput =
    [ [ text "A higher burn means more visibility!" ]
        |> paragraph
            [ Font.size 14
            , spacing 3
            , Font.color white
            , Font.italic
            , Font.center
            , width fill
            ]
    , [ View.Img.dollar 26 white
      , Input.text
            [ View.Attrs.whiteGlowAttributeSmall
            , Background.color <| Element.rgb 0 0 0
            , Font.color white
            , width <| px 60
            , height <| px 34
            , padding 3
            , Font.size 26
            ]
            { onChange = ComposeDollarChange
            , label = Input.labelHidden ""
            , placeholder = Just <| Input.placeholder [] <| text "0.00"
            , text = amountInput
            }
      ]
        |> row [ spacing 5 ]
    ]
        |> row
            [ spacing 5
            , padding 5
            , Background.color <| Element.rgb 0.4 0.2 0.2
            , roundBorder
            , View.Attrs.cappedWidth 300
            ]


viewMarkdown : Model -> Element Msg
viewMarkdown model =
    let
        isMobile =
            model.dProfile == Helpers.Element.Mobile
    in
    if isMobile then
        if model.compose.preview then
            model.compose.body
                |> View.Markdown.renderString model.dProfile
                |> el
                    [ width fill
                    , height fill
                    , Element.scrollbarY
                    , whiteGlowAttributeSmall
                    , Font.color white
                    , padding 10
                    ]
                |> List.singleton
                |> column [ width fill, height fill ]

        else
            Input.multiline
                [ if isMobile then
                    width fill

                  else
                    width <| px 500
                , if isMobile then
                    height fill

                  else
                    height <| px 500
                , View.Attrs.whiteGlowAttributeSmall
                , Background.color black
                , Font.color white
                , height <| px 0
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
                |> el
                    [ Html.Attributes.class "multiline"
                        |> Element.htmlAttribute
                    , Element.scrollbarY
                    , height fill
                    , width fill
                    ]

    else
        [ Input.multiline
            [ width <| px 500
            , height <| px 500
            , View.Attrs.whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
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

                --, View.Attrs.scrollFix
                , whiteGlowAttributeSmall
                , Font.color white
                , padding 10
                ]
        ]
            |> row
                [ height fill
                , width fill
                , spacing 30
                ]
