module View.Post exposing (view)

import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, padding, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile, black, white)
import Helpers.Time as TimeHelpers
import Maybe.Extra exposing (unwrap)
import Misc
import Set exposing (Set)
import Theme exposing (almostWhite, theme)
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, typeFont, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement, when, whenJust)
import View.Img
import View.Markdown


view :
    DisplayProfile
    -> Maybe Posix
    -> Posix
    -> Set.Set Types.PostKey
    -> Maybe Accounting
    -> Maybe ShowInputState
    -> String
    -> Maybe String
    -> Maybe UserInfo
    -> CoreData
    -> Element Msg
view dProfile timestamp now replies accounting state input topic wallet post =
    let
        isMobile =
            dProfile == EH.Mobile

        block =
            "Block "
                ++ String.fromInt post.id.block
                |> text

        timing =
            viewTiming timestamp now

        showActions =
            wallet
                |> unwrap False (.chain >> (==) post.chain)
    in
    [ [ accounting
            |> whenJust (viewAccounting dProfile)
      , [ topic
            |> whenJust
                (\t ->
                    Input.button [ Font.size 30, hover ]
                        { onPress = Just <| GotoView <| ViewTopic t
                        , label =
                            "#"
                                ++ t
                                |> text
                        }
                )
            |> el []
        , viewCard timestamp now post
            |> when (not isMobile)
        ]
            |> row [ width fill, spaceEvenly ]
      ]
        |> row [ width fill, spacing 10 ]
    , [ block
      , timing
      ]
        |> column [ width fill, spacing 10 ]
        |> when isMobile
    , [ phaceElement
            ( 60, 60 )
            False
            post.author
            False
            ClickHappened
      , [ viewContent post
            |> linkToPost post.id
        , [ [ View.Img.speechBubble 17 almostWhite
            , text <| viewReplies replies
            ]
                |> row [ spacing 10, Font.size 23 ]
                |> linkToPost post.id
                |> el
                    [ Font.color almostWhite
                    , Font.size 17
                    , width fill
                    , Element.alignTop
                    ]
          , viewActions post input state
                |> when showActions
          ]
            |> row [ width fill, spaceEvenly ]
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


viewCard : Maybe Time.Posix -> Time.Posix -> CoreData -> Element Msg
viewCard timestamp now post =
    let
        block =
            "@"
                ++ String.fromInt post.id.block
                |> text

        timing =
            viewTiming timestamp now

        col =
            case post.chain of
                Types.XDai ->
                    Theme.softRed

                Types.Eth ->
                    Theme.orange
    in
    Element.newTabLink
        [ hover
        , Background.color col
        , Font.color white
        , roundBorder
        , padding 10
        , View.Attrs.sansSerifFont
        , width <| px 270
        ]
        { url = Misc.txUrl post.chain post.txHash
        , label =
            [ [ View.Common.viewChain post.chain
              , block
              ]
                |> column [ spacing 10, width fill ]
            , View.Common.verticalRule white
            , timing
                |> el [ width fill ]
            ]
                |> row
                    [ spacing 10
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


viewContent : CoreData -> Element Msg
viewContent post =
    [ post.content.title |> whenJust (text >> el [ Font.bold ])
    , post.content.desc |> whenJust (text >> el [ Font.italic ])
    , post.content.body
        |> View.Markdown.renderString
        |> el
            [ height <| px 100
            , Element.clip
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


viewAccounting : DisplayProfile -> Accounting -> Element Msg
viewAccounting _ accounting =
    Element.row
        [ spacing 5
        ]
        [ viewAmount theme.daiBurnedBackground accounting.totalBurned
        , viewAmount theme.daiTippedBackground accounting.totalTipped
        ]


viewAmount : Color -> TokenValue -> Element Msg
viewAmount color amount =
    [ View.Img.dollar 22 white
    , Misc.formatDollar amount
        |> text
    ]
        |> row
            [ Element.padding 3
            , Element.Border.rounded 3
            , Font.size 22
            , Font.color white
            , Background.color color
            ]


viewTiming : Maybe Time.Posix -> Time.Posix -> Element Msg
viewTiming maybePostTime now =
    maybePostTime
        |> unwrap
            (View.Img.spinner 20 white
                |> el [ centerX, View.Attrs.rotate ]
            )
            (\time ->
                TimeHelpers.sub now time
                    |> TimeHelpers.roundToSingleUnit
                    |> (\s -> s ++ " ago")
                    |> text
                    |> el [ View.Attrs.title (Misc.formatPosix time) ]
            )


viewActions : CoreData -> String -> Maybe ShowInputState -> Element Msg
viewActions post input =
    unwrap
        ([ supportTipButton post.id
         , supportBurnButton post.id
         ]
            |> row [ spacing 10 ]
        )
        (\showInput ->
            let
                title =
                    case showInput of
                        Tip _ ->
                            "Tip Ether for this post, rewarding the author."

                        Burn _ ->
                            "Burn Ether to increase the visibility of this post."

                msg =
                    case showInput of
                        Tip _ ->
                            SubmitTip post.id

                        Burn _ ->
                            --"Burn DAI to increase this post's visibility"
                            SubmitBurn post.id
            in
            [ text title
            , [ View.Img.dollar 30 white
              , Input.text [ Font.color black ]
                    { onChange = ComposeDollarChange
                    , label = Input.labelHidden ""
                    , placeholder =
                        "00.00"
                            |> text
                            |> Input.placeholder []
                            |> Just
                    , text = input
                    }
              ]
                |> row [ spacing 5, width fill ]
            , [ Input.button
                    [ Font.underline
                    , hover
                    ]
                    { onPress = Just CancelTipOpen
                    , label = text "Cancel"
                    }
              , Input.button
                    [ Background.color Theme.orange
                    , padding 10
                    , roundBorder
                    , hover
                    , Font.color black
                    ]
                    { onPress = Just msg
                    , label = text "Submit"
                    }
              ]
                |> row [ Element.alignRight, spacing 20 ]
            ]
                |> column
                    [ Background.color black
                    , spacing 20
                    , padding 10
                    , width fill
                    , Font.color white
                    , View.Attrs.sansSerifFont
                    ]
        )


supportTipButton :
    PostId
    -> Element Msg
supportTipButton postId =
    Input.button
        [ height <| px 40
        , Background.color theme.daiTippedBackground
        , width <| px 40
        , EH.withTitle "Tip DAI for this post, rewarding the author"
        , hover
        ]
        { onPress = Just <| SetTipOpen <| PostState postId (Types.Tip "")
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }


supportBurnButton :
    PostId
    -> Element Msg
supportBurnButton postId =
    Input.button
        [ height <| px 40
        , Background.color theme.daiBurnedBackground
        , width <| px 40
        , EH.withTitle "Burn DAI to increase this post's visibility"
        , hover
        ]
        { onPress = Just <| SetTipOpen <| PostState postId (Types.Burn "")
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }
