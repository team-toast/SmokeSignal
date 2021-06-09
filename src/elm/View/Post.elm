module View.Post exposing (view, viewBurnOrTip, viewChainCard, viewReplyInput)

import Chain
import Dict exposing (Dict)
import Element exposing (Color, Element, alignBottom, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Maybe.Extra exposing (unwrap)
import Misc
import Set exposing (Set)
import Theme exposing (almostWhite, black, orange, white)
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (cappedWidth, hover, roundBorder, sansSerifFont, slightRound, typeFont, whiteGlowAttributeSmall)
import View.Common exposing (chain, phaceElement, when, whenAttr, whenJust)
import View.Compose
import View.Img
import View.Markdown


view :
    DisplayProfile
    -> Maybe Posix
    -> Posix
    -> Dict PostKey (Set PostKey)
    -> Dict PostKey Accounting
    -> Maybe BurnOrTipUX
    -> Maybe TooltipId
    -> Maybe String
    -> Maybe UserInfo
    -> Core
    -> Element Msg
view dProfile timestamp now replies accountingDict state tooltipState topic wallet post =
    let
        accounting =
            countAccounting accountingDict replies post.key

        replyCount =
            countReplies replies post.key

        isMobile =
            dProfile == Mobile

        pad =
            if isMobile then
                10

            else
                30
    in
    [ viewHeader isMobile tooltipState accounting topic timestamp now post
    , viewCardMobile timestamp now post
        |> when isMobile
    , viewBody dProfile post
    , viewBottom replyCount post wallet state
    ]
        |> column
            [ width fill
            , spacing 10
            , Background.color black
            , Font.color white
            , whiteGlowAttributeSmall
            , padding pad
            , typeFont
            ]


emptyAccounting : Accounting
emptyAccounting =
    { firstAuthor = Misc.emptyAddress
    , totalBurned = TokenValue.zero
    , totalTipped = TokenValue.zero
    }


addAccounting : Accounting -> Accounting -> Accounting
addAccounting a b =
    { a
        | totalBurned = TokenValue.add a.totalBurned b.totalBurned
        , totalTipped = TokenValue.add a.totalTipped b.totalTipped
    }


countAccounting : Dict PostKey Accounting -> Dict PostKey (Set PostKey) -> PostKey -> Accounting
countAccounting accounting replies pk =
    let
        curr =
            accounting
                |> Dict.get pk
                |> Maybe.withDefault emptyAccounting
    in
    replies
        |> Dict.get pk
        |> unwrap curr
            (\set ->
                set
                    |> Set.toList
                    |> List.map (countAccounting accounting replies)
                    |> List.foldl addAccounting curr
            )


countReplies : Dict PostKey (Set PostKey) -> PostKey -> Int
countReplies replies pk =
    replies
        |> Dict.get pk
        |> unwrap 0
            (\set ->
                set
                    |> Set.toList
                    |> List.map (countReplies replies)
                    |> List.sum
                    |> (+) (Set.size set)
            )


viewBottom : Int -> Core -> Maybe UserInfo -> Maybe BurnOrTipUX -> Element Msg
viewBottom replyCount post wallet state =
    [ [ View.Img.speechBubble 17 almostWhite
      , replyCount
            |> Misc.formatReplies
            |> text
      ]
        |> row [ spacing 10, Font.size 23 ]
        |> el
            [ Element.alignLeft
            , Element.alignBottom
            ]
        |> el
            [ Font.color almostWhite
            , Font.size 17
            , width fill
            , height fill
            ]
        |> linkToPost post.id
        |> when (state == Nothing)
    , viewBurnOrTip post wallet state
    ]
        |> row [ width fill, spacing 10 ]


viewHeader : Bool -> Maybe TooltipId -> Accounting -> Maybe String -> Maybe Time.Posix -> Time.Posix -> Core -> Element Msg
viewHeader isMobile tooltipState accounting topic timestamp now post =
    [ phaceElement
        60
        post.author
        False
        (GotoView <| ViewUser post.author)
        |> el [ Element.alignTop ]
    , [ post.content.title
            |> whenJust
                (text
                    >> List.singleton
                    >> paragraph [ Font.size 30 ]
                    >> linkToPost post.id
                )
      , [ viewAccounting tooltipState post accounting
        , [ topic
                |> whenJust
                    (\t ->
                        Input.button [ Font.size 20, hover, width fill ]
                            { onPress = Just <| GotoView <| ViewTopic t
                            , label = View.Common.topic t
                            }
                    )
          , View.Common.timingOrSpinner now timestamp
          ]
            |> row [ spacing 10 ]
            |> when (not isMobile)
        ]
            |> row [ width fill, spaceEvenly ]
      ]
        |> column [ width fill, spacing 20 ]
    , viewChainCard () post
        |> el [ Element.alignTop ]
        |> when (not isMobile)
    ]
        |> row [ width fill, spacing 20 ]


viewAccounting : Maybe TooltipId -> Core -> Accounting -> Element Msg
viewAccounting tooltipState post data =
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
                                        "Author earned $" ++ Misc.formatDollar data.totalTipped ++ " for this post"

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


viewBurnOrTip : Core -> Maybe UserInfo -> Maybe BurnOrTipUX -> Element Msg
viewBurnOrTip post chain =
    let
        isActiveWalletChain =
            chain
                |> unwrap False (.chain >> (==) post.chain)

        buttons =
            [ supportBurnButton post.id
            , supportTipButton post.id
            ]
                |> row [ spacing 10, Element.alignRight ]
                |> when isActiveWalletChain
    in
    unwrap
        buttons
        (\data ->
            if data.id == post.id then
                viewBurnOrTipInput post data

            else
                buttons
        )


viewChainCard : a -> Core -> Element Msg
viewChainCard _ post =
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
        , Background.color <| Theme.withAlpha 0.2 col
        , Border.width 1
        , Border.color <| Theme.withAlpha 0.5 <| col
        , Font.color <| Theme.withAlpha 0.5 <| Theme.white
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
            View.Common.timingOrSpinner now timestamp

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


viewBody : DisplayProfile -> Core -> Element Msg
viewBody device post =
    post.content.body
        |> View.Markdown.renderString device
        |> el
            [ height <| px 100
            , View.Attrs.sansSerifFont
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
        |> linkToPost post.id


viewAmount : Color -> TokenValue -> TooltipId -> Element Msg
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
        { onPress = Just <| ToggleTooltip state
        , label =
            [ View.Img.dollar 22 white
            , Misc.formatDollar amount
                |> text
                |> el [ Element.moveUp 1 ]
            ]
                |> row []
        }


viewBurnOrTipInput : Core -> BurnOrTipUX -> Element Msg
viewBurnOrTipInput post state =
    let
        name =
            Chain.getName post.chain

        title =
            case state.burnOrTip of
                Tip ->
                    "Tip " ++ name ++ " for this post, rewarding the author."

                Burn ->
                    "Burn " ++ name ++ " to increase the visibility of this post."

        customAmountMsg =
            if state.inProgress then
                Nothing

            else
                state.input
                    |> Maybe.andThen String.toFloat
                    |> Maybe.andThen
                        (\amount ->
                            if amount >= 0 then
                                Just <| SubmitTipOrBurn amount

                            else
                                Nothing
                        )
    in
    [ [ text title ]
        |> paragraph [ Font.alignRight ]
    , state.input
        |> unwrap
            (viewCurrencyButtons state)
            (\txt ->
                [ View.Img.dollar 30 white
                , Input.text [ Font.color black, cappedWidth 400 ]
                    { onChange = BurnOrTipUXInputChange
                    , label = Input.labelHidden ""
                    , placeholder =
                        "00.00"
                            |> text
                            |> Input.placeholder []
                            |> Just
                    , text = txt
                    }
                ]
                    |> row [ spacing 5, Element.alignRight ]
            )
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
    , [ View.Common.spinner 20 white
            |> when state.inProgress
      , View.Common.cancel CancelPostInput
      , Input.button
            [ Background.color Theme.orange
            , padding 10
            , roundBorder
            , hover
                |> whenAttr (not state.inProgress)
            , Font.color black
            ]
            { onPress = customAmountMsg
            , label = text "Submit"
            }
            |> when (state.input /= Nothing)
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


viewCurrencyButtons : BurnOrTipUX -> Element Msg
viewCurrencyButtons state =
    let
        color =
            case state.burnOrTip of
                Tip ->
                    Theme.darkGreen

                Burn ->
                    Theme.darkRed
    in
    [ 0.1
    , 0.5
    , 1
    , 5
    ]
        |> List.map
            (\n ->
                let
                    txt =
                        if n < 1 then
                            (n * 100)
                                |> round
                                |> String.fromInt
                                |> (\str -> str ++ "¢")

                        else
                            round n
                                |> String.fromInt
                                |> (++) "$"
                in
                viewCurrencyButton state.inProgress
                    color
                    (SubmitTipOrBurn n)
                    (text txt)
            )
        |> (\xs ->
                xs
                    ++ [ viewCurrencyButton state.inProgress
                            color
                            (BurnOrTipUXInputChange "")
                            (View.Img.pencil 20 white)
                       ]
           )
        |> row [ spacing 10, Element.alignRight ]


viewCurrencyButton : Bool -> Color -> msg -> Element msg -> Element msg
viewCurrencyButton inProgress color msg label =
    Input.button
        [ width <| px 45
        , height <| px 45
        , Background.color color
        , roundBorder
        , hover
            |> whenAttr (not inProgress)
        ]
        { onPress =
            if inProgress then
                Nothing

            else
                Just msg
        , label =
            label
                |> el [ centerX, centerY ]
        }


supportTipButton : PostId -> Element Msg
supportTipButton postId =
    Input.button
        [ height <| px 40
        , Background.color Theme.darkGreen
        , width <| px 40
        , View.Attrs.title "Tip for this post, rewarding the author."
        , hover
        ]
        { onPress = Just <| StartBurnOrTipUX postId Types.Tip
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
        , View.Attrs.title "Burn to increase the visibility of this post."
        , hover
        ]
        { onPress = Just <| StartBurnOrTipUX postId Types.Burn
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }


viewReplyInput : Bool -> DisplayProfile -> ComposeModel -> UserInfo -> Element Msg
viewReplyInput a b c userInfo =
    View.Compose.composePanel False (viewReplyLabel userInfo.chain) a b c userInfo


viewReplyLabel : Chain -> Element msg
viewReplyLabel chain =
    [ [ View.Img.replyArrow 25 orange
      , "Reply with"
            |> text
            |> el [ Font.color orange ]
      ]
        |> row [ spacing 10 ]
    , View.Common.chain chain
        |> el
            [ Background.color white
            , View.Attrs.roundBorder
            , padding 5
            , Font.color black
            ]
    ]
        |> row [ spacing 10, Element.moveUp 5 ]
