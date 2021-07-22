module View.Compose exposing (composePanel, view)

import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Maybe.Extra
import Theme exposing (black, orange, white)
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (when, whenAttr, whenJust)
import View.Img
import View.Markdown
import Wallet


view : Model -> Element Msg
view model =
    [ "Compose New Post"
        |> text
        |> el [ Font.size 35, Font.color black ]
        |> el
            [ width fill
            , Background.color orange
            , Font.color white
            , padding 15
            , View.Attrs.whiteGlowAttribute
            ]
        |> when (model.dProfile == Desktop)
    , model.wallet
        |> Wallet.userInfo
        |> Maybe.Extra.unwrap
            (Input.button
                [ Background.color Theme.orange
                , padding 10
                , View.Attrs.roundBorder
                , hover
                , Font.color black
                , centerX
                , centerY
                ]
                { onPress = Just ConnectToWeb3
                , label =
                    if model.wallet == Connecting then
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        [ text "Connect wallet to compose post" ]
                            |> paragraph [ Font.center ]
                }
                |> el
                    [ Background.color black
                    , whiteGlowAttributeSmall
                    , height <| px 150
                    , width <| px 240
                    , padding 40
                    , centerX
                    ]
                |> el [ width fill, height fill, padding 20 ]
            )
            (composePanel
                True
                ([ "Topic"
                    |> text
                    |> el [ centerY ]
                    |> el
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
                 , Input.text
                    [ Background.color white
                    , width <| px 250
                    , Border.roundEach
                        { bottomLeft = 0
                        , topLeft = 0
                        , bottomRight = 5
                        , topRight = 5
                        }
                    , padding 5
                    , height <| px 35
                    , Element.Events.onLoseFocus SanitizeTopic
                    ]
                    { onChange = TopicInputChange
                    , label = Input.labelHidden ""
                    , placeholder =
                        text "Choose topic"
                            |> Input.placeholder []
                            |> Just
                    , text = model.topicInput
                    }
                 ]
                    |> row [ Element.paddingXY 0 3 ]
                )
                model.chainSwitchInProgress
                model.dProfile
                model.compose
            )
    ]
        |> column [ height fill, width fill, spacing 10 ]


composePanel : Bool -> Element Msg -> Bool -> DisplayProfile -> ComposeModel -> UserInfo -> Element Msg
composePanel isCompose elem chainSwitchInProgress dProfile compose userInfo =
    let
        isMobile =
            dProfile == Mobile

        submitEnabled =
            not (String.isEmpty compose.body)
                && not compose.inProgress

        topButton txt val =
            let
                active =
                    val == compose.preview
            in
            Input.button
                [ padding 10
                , Background.color orange
                    |> whenAttr active
                , Element.alignRight
                , Border.roundEach
                    { bottomLeft = 0
                    , topLeft = 5
                    , bottomRight = 0
                    , topRight = 5
                    }
                , hover
                    |> whenAttr (not active)
                , sansSerifFont
                , if active then
                    Font.color black

                  else
                    Font.color white
                , Font.bold
                ]
                { onPress = Just <| PreviewSet val
                , label = text txt
                }
    in
    [ [ [ topButton "Write" False
        , topButton "Preview" True
        ]
            |> row [ spacing 10, Element.paddingXY 10 0 ]
            |> when (isMobile || not isCompose)
      , elem
            |> when (not isMobile)
            |> el [ Element.alignRight ]
      ]
        |> row [ width fill, spaceEvenly ]
    , [ View.Common.viewInstructions chainSwitchInProgress dProfile userInfo
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
            , text = compose.title
            }
            |> when isCompose
      , viewMarkdown isCompose dProfile compose
      , compose.error
            |> whenJust
                ((\txt ->
                    [ text txt
                    , Input.button [ hover ]
                        { onPress = Just CloseComposeError
                        , label = View.Img.close 25 black
                        }
                    ]
                 )
                    >> row
                        [ Background.color white
                        , slightRound
                        , padding 10
                        , spacing 10
                        , Font.color black
                        , Element.alignBottom
                        , Element.alignRight
                        ]
                )
      , [ viewBurnAmountUX compose.burnAmount
        , [ View.Common.cancel
                (if isCompose then
                    GotoView ViewHome

                 else
                    ComposeClose
                )
                |> el [ Font.color black ]
          , Input.button
                [ Background.color Theme.green
                , Font.bold
                , Font.size 25
                , Element.alignRight
                , View.Attrs.roundBorder
                , if submitEnabled then
                    hover

                  else
                    View.Attrs.notAllowed
                , sansSerifFont
                , width <| px 100
                , height <| px 50
                ]
                { onPress =
                    if submitEnabled then
                        Just SubmitDraft

                    else
                        Nothing
                , label =
                    if compose.inProgress then
                        View.Common.spinner 20 white
                            |> el [ centerX, centerY ]

                    else
                        text "Submit"
                            |> el [ centerX, centerY ]
                }
          ]
            |> row [ Element.alignRight, spacing 10 ]
        ]
            |> row [ width fill, spacing 10 ]
      ]
        |> column
            [ width fill
            , height fill
            , spacing 10
            , padding 10
            , roundBorder
            , Background.color orange
            ]
    ]
        |> column
            [ height fill
            , width fill
            ]


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


viewMarkdown : Bool -> DisplayProfile -> ComposeModel -> Element Msg
viewMarkdown isCompose dProfile compose =
    let
        heightLen =
            if isCompose then
                fill

            else
                px 200

        isMobile =
            dProfile == Mobile

        preview =
            (if String.isEmpty compose.body then
                text "Nothing to preview"

             else
                compose.body
                    |> View.Markdown.renderString dProfile
            )
                |> el
                    [ width fill
                    , height heightLen
                    , Element.scrollbarY
                    , Font.color white
                    , padding 10
                    , Background.color black
                    ]
                |> View.Common.scrollbarYHack

        input =
            Input.multiline
                [ width fill
                , height fill
                , Font.color white
                , Border.width 0
                , Border.rounded 0
                , Background.color black
                ]
                { onChange = Types.ComposeBodyChange
                , label = Input.labelHidden ""
                , placeholder =
                    "What do you want to say?"
                        |> text
                        |> Input.placeholder []
                        |> Just
                , text = compose.body
                , spellcheck = True
                }
                |> el
                    [ Html.Attributes.class "multiline"
                        |> Element.htmlAttribute
                    , Element.scrollbarY
                    , height heightLen
                    , width fill
                    ]
    in
    if isMobile || not isCompose then
        if compose.preview then
            preview

        else
            input

    else
        [ input
            |> View.Common.scrollbarYHack
        , preview
        ]
            |> row [ width fill, height fill, spacing 20 ]
