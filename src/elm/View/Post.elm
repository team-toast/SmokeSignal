module View.Post exposing (view, viewCard, viewTipOrBurn)

import Chain
import Element exposing (Color, Element, alignBottom, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile, black, white)
import Maybe.Extra exposing (unwrap)
import Misc
import Set exposing (Set)
import Theme exposing (almostWhite)
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, slightRound, typeFont, whiteGlowAttributeSmall)
import View.Common exposing (chain, phaceElement, when, whenJust)
import View.Img
import View.Markdown


view :
    DisplayProfile
    -> Maybe Posix
    -> Posix
    -> Set.Set Types.PostKey
    -> Maybe Accounting
    -> Maybe PostState
    -> Maybe TooltipState
    -> Maybe String
    -> Maybe UserInfo
    -> Core
    -> Element Msg
view dProfile timestamp now replies accounting state tooltipState topic wallet post =
    let
        isMobile =
            dProfile == EH.Mobile

        tipOrBurn =
            viewTipOrBurn post wallet state
    in
    [ [ [ accounting
            |> whenJust
                (\data ->
                    [ viewAmount Theme.darkRed
                        data.totalBurned
                        { id = post.id, labelType = Burn }
                    , viewAmount Theme.darkGreen
                        data.totalTipped
                        { id = post.id, labelType = Tip }
                    ]
                        |> row
                            [ spacing 5
                            , tooltipState
                                |> whenJust
                                    (\val ->
                                        let
                                            txt =
                                                case val.labelType of
                                                    Tip ->
                                                        "The author has been tipped $" ++ Misc.formatDollar data.totalTipped ++ " for this post"

                                                    Burn ->
                                                        "Author burned $" ++ Misc.formatDollar post.authorBurn ++ " + crowd amplified $" ++ Misc.formatDollar (TokenValue.sub data.totalBurned post.authorBurn)
                                        in
                                        [ text txt ]
                                            |> paragraph
                                                [ padding 10
                                                , (if val.labelType == Burn then
                                                    Theme.darkRed

                                                   else
                                                    Theme.darkGreen
                                                  )
                                                    |> Background.color
                                                ]
                                            |> when (val.id == post.id)
                                    )
                                |> Element.below
                            ]
                )
        , View.Common.timing now timestamp
            |> when (not isMobile)
        ]
            |> column [ spacing 10 ]
      , [ topic
            |> whenJust
                (\t ->
                    Input.button [ Font.size 30, hover, width fill ]
                        { onPress = Just <| GotoView <| ViewTopic t
                        , label = View.Common.topic t
                        }
                )
            |> el [ width fill, Element.alignTop ]
        ]
            |> row [ width fill, spaceEvenly, Element.alignTop ]
      , viewCard post
            |> el [ Element.alignTop ]
            |> when (not isMobile)
      ]
        |> row [ width fill, spacing 10 ]
    , viewCardMobile timestamp now post
        |> when isMobile
    , [ phaceElement
            60
            post.author
            False
            (GotoView <| ViewUser post.author)
            |> el [ Element.alignTop ]
      , [ viewContent dProfile post
            |> linkToPost post.id
        , [ [ View.Img.speechBubble 17 almostWhite
            , viewReplies replies
                |> text
            ]
                |> row [ spacing 10, Font.size 23 ]
                |> linkToPost post.id
                |> el
                    [ Element.alignRight
                    , Element.alignBottom
                    ]
                |> el
                    [ Font.color almostWhite
                    , Font.size 17
                    , width fill
                    , height fill
                    ]
          , tipOrBurn
          ]
            |> (if isMobile then
                    column [ width fill, spacing 10 ]

                else
                    row [ width fill, spacing 10 ]
               )
        ]
            |> column
                [ spacing 10
                , width fill
                ]
      ]
        |> row [ width fill, spacing 10 ]
    ]
        |> column
            [ width fill
            , spacing 10
            ]
        |> el
            [ Background.color black
            , Font.color white
            , whiteGlowAttributeSmall
            , padding 10
            , width fill
            , typeFont
            ]


viewTipOrBurn : Core -> Maybe UserInfo -> Maybe PostState -> Element Msg
viewTipOrBurn post chain =
    let
        showActions =
            chain
                |> unwrap False (.chain >> (==) post.chain)
    in
    unwrap
        (viewButtons post
            |> when showActions
        )
        (\data ->
            viewTipOrBurnInput post data
                |> when (data.id == post.id)
        )


viewCard : Core -> Element Msg
viewCard post =
    let
        block =
            "@"
                ++ String.fromInt post.id.block
                |> text

        col =
            Chain.getColor post.chain
    in
    Element.newTabLink
        [ hover
        , Background.color <| EH.withAlpha 0.2 col
        , Border.width 1
        , Border.color <| EH.withAlpha 0.5 <| col
        , Font.color <| EH.withAlpha 0.5 <| EH.white
        , Element.paddingXY 10 10
        , View.Attrs.sansSerifFont
        ]
        { url = Chain.txUrl post.chain post.txHash
        , label =
            [ [ chain post.chain
                    |> el [ Font.bold, Font.color col ]
              , [ block
                    |> el [ Font.size 14, alignBottom ]
                , View.Img.link 17 (Element.rgb 0.8 0.8 0.8)
                    |> el [ Element.alignRight ]
                ]
                    |> row [ spacing 5, width fill ]
              ]
                |> column [ spacing 5, Font.size 20 ]
            ]
                |> row
                    [ spacing 10
                    , Font.size 17
                    , width fill
                    ]
        }


viewCardMobile : Maybe Time.Posix -> Time.Posix -> Core -> Element Msg
viewCardMobile timestamp now post =
    let
        block =
            "@"
                ++ String.fromInt post.id.block
                |> text

        timing =
            View.Common.timing now timestamp

        col =
            Chain.getColor post.chain
    in
    Element.newTabLink
        [ hover
        , Background.color col
        , Font.color white
        , roundBorder
        , padding 10
        , View.Attrs.sansSerifFont
        , width fill
        ]
        { url = Chain.txUrl post.chain post.txHash
        , label =
            [ View.Common.chain post.chain
            , View.Common.verticalRule white
            , block
            , View.Common.verticalRule white
            , timing
            ]
                |> row
                    [ spaceEvenly
                    , Font.size 17
                    , width fill
                    ]
        }


linkToPost : PostId -> Element Msg -> Element Msg
linkToPost id elem =
    Input.button [ width fill, hover ]
        { onPress = Just <| GotoView <| ViewPost id
        , label = elem
        }


viewContent : DisplayProfile -> Core -> Element Msg
viewContent device post =
    [ post.content.title |> whenJust (text >> List.singleton >> paragraph [ Font.bold ])
    , post.content.desc |> whenJust (text >> List.singleton >> paragraph [ Font.italic ])
    , post.content.body
        |> View.Markdown.renderString device
        |> el
            [ height <| px 100
            , Element.clip
            , width fill
            , el
                [ View.Attrs.cappedHeight 50
                , width fill
                , Element.alignBottom
                , Background.gradient
                    { angle = degrees 0
                    , steps =
                        [ 0.9
                        , 0.8
                        , 0.7
                        , 0.6
                        , 0.5
                        , 0.4
                        , 0.3
                        ]
                            |> List.map Theme.blackAlpha
                    }
                ]
                Element.none
                |> Element.inFront
            ]
    ]
        |> column
            [ width fill
            , View.Attrs.sansSerifFont
            , spacing 10
            ]


viewReplies : Set a -> String
viewReplies replies =
    let
        len =
            Set.size replies

        word =
            if len == 1 then
                "reply"

            else
                "replies"
    in
    String.fromInt len ++ " " ++ word


viewAmount : Color -> TokenValue -> TooltipState -> Element Msg
viewAmount color amount state =
    Input.button
        [ padding 5
        , Border.rounded 3
        , Font.size 22
        , Font.color white
        , Background.color color
        , hover
        , View.Attrs.help
        ]
        { onPress = Just <| SetTooltipState state
        , label =
            [ View.Img.dollar 22 white
            , Misc.formatDollar amount
                |> text
                |> el [ Element.moveUp 1 ]
            ]
                |> row []
        }


viewTipOrBurnInput : Core -> PostState -> Element Msg
viewTipOrBurnInput post state =
    let
        name =
            Chain.getName post.chain

        title =
            case state.txType of
                Tip ->
                    "Tip " ++ name ++ " for this post, rewarding the author."

                Burn ->
                    "Burn " ++ name ++ " to increase the visibility of this post."

        isEmpty =
            String.isEmpty state.input
                || (state.input
                        |> String.toFloat
                        |> unwrap False ((==) 0.0)
                   )
    in
    [ [ text title ]
        |> paragraph []
    , [ View.Img.dollar 30 white
      , Input.text [ Font.color black ]
            { onChange = PostInputChange
            , label = Input.labelHidden ""
            , placeholder =
                "00.00"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = state.input
            }
      ]
        |> row [ spacing 5, width fill ]
    , state.error
        |> whenJust
            (text
                >> List.singleton
                >> paragraph
                    [ Background.color white
                    , Element.alignRight
                    , slightRound
                    , padding 10
                    , Font.color black
                    ]
            )
    , [ View.Common.cancel CancelPostInput
      , Input.button
            [ Background.color Theme.orange
            , padding 10
            , roundBorder
            , hover
            , Font.color black
            ]
            { onPress =
                if state.inProgress || isEmpty then
                    Nothing

                else
                    Just SubmitTipOrBurn
            , label =
                if state.inProgress then
                    View.Common.spinner 20 black
                        |> el [ centerX ]

                else
                    text "Submit"
            }
      ]
        |> row [ Element.alignRight, spacing 20 ]
    ]
        |> column
            [ Background.color black
            , spacing 10
            , padding 10
            , width fill
            , Font.color white
            , View.Attrs.sansSerifFont
            ]


viewButtons : Core -> Element Msg
viewButtons post =
    [ supportBurnButton post.id
    , supportTipButton post.id
    ]
        |> row [ spacing 10, Element.alignRight ]


supportTipButton : PostId -> Element Msg
supportTipButton postId =
    Input.button
        [ height <| px 40
        , Background.color Theme.darkGreen
        , width <| px 40
        , EH.withTitle "Tip for this post, rewarding the author."
        , hover
        ]
        { onPress = Just <| SetPostInput postId Types.Tip
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }


supportBurnButton : PostId -> Element Msg
supportBurnButton postId =
    Input.button
        [ height <| px 40
        , Background.color Theme.darkRed
        , width <| px 40
        , EH.withTitle "Burn to increase the visibility of this post."
        , hover
        ]
        { onPress = Just <| SetPostInput postId Types.Burn
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }
