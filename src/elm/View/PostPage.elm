module View.PostPage exposing (view)

import Chain
import Dict exposing (Dict)
import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Utils
import Html.Attributes
import Maybe.Extra exposing (unwrap)
import Misc
import Set
import Theme exposing (black, orange, white)
import TokenValue
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (phaceElement, when, whenAttr, whenJust)
import View.Compose
import View.Img
import View.Markdown
import View.Post
import Wallet


view : Model -> LogPost -> Element Msg
view model post =
    let
        core =
            Misc.getCore post
    in
    [ viewBreadcrumbs post model.rootPosts model.replyPosts
    , viewPost model core
    , viewReplies model core
    ]
        |> column
            [ height fill
            , spacing 20
            , width fill
            , sansSerifFont
            ]


viewPost : Model -> Core -> Element Msg
viewPost model core =
    let
        pad =
            if isMobile then
                15

            else
                30

        isMobile =
            model.dProfile == Mobile
    in
    [ viewHeader model core
    , View.Common.horizontalRule white
    , core.content.body
        |> View.Markdown.renderString model.dProfile
        |> el
            [ width fill
            , height fill
            , Font.color white
            ]
    , viewBottomBar model core
    ]
        |> column
            [ spacing 20
            , width fill
            , padding pad
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            ]


viewHeader : Model -> Core -> Element Msg
viewHeader model core =
    let
        accounting =
            model.accounting
                |> Dict.get core.key
                |> View.Common.whenJust (viewAccounting model.dProfile)

        isMobile =
            model.dProfile == Mobile

        fontSize =
            if isMobile then
                25

            else
                50
    in
    [ core.content.title
        |> View.Common.whenJust
            (text
                >> List.singleton
                >> Element.paragraph
                    [ Font.size fontSize
                    , Font.bold
                    ]
            )
    , [ [ accounting
        , phaceElement
            70
            core.author
            (model.showAddressId == Just (PhaceForPublishedPost core.id))
            (GotoView <| ViewUser core.author)
        , View.Post.viewChainCard model.dProfile core
        ]
            |> row [ spacing 10 ]
      , [ model.blockTimes
            |> Dict.get core.id.block
            |> View.Common.timingOrSpinner model.now
        , Element.newTabLink [ hover ]
            { url = Chain.txUrl core.chain core.txHash
            , label =
                [ View.Img.globe 20 white, text "View on block explorer" ]
                    |> row [ spacing 5, Font.underline ]
            }
        ]
            |> column
                [ spacing 10
                ]
      ]
        |> (if isMobile then
                column [ spacing 10 ]

            else
                row [ width fill, Element.spaceEvenly ]
           )
    ]
        |> column [ spacing 20, width fill ]


viewRepliesHeader : Dict PostKey Accounting -> List ReplyPost -> Element Msg
viewRepliesHeader accounting replies =
    let
        totalBurn =
            replies
                |> List.map
                    (\reply ->
                        Dict.get reply.core.key accounting
                            |> unwrap reply.core.authorBurn .totalBurned
                    )
                |> List.foldl
                    TokenValue.add
                    TokenValue.zero
    in
    [ replies
        |> List.length
        |> Misc.formatReplies
        |> text
        |> el [ Font.color white ]
    , View.Common.burn totalBurn
    ]
        |> row
            [ spacing 20
            , padding 10
            , Background.color black
            , whiteGlowAttributeSmall
            , Element.alignRight
            ]


viewReplies : Model -> Core -> Element Msg
viewReplies model core =
    let
        userInfo =
            model.wallet
                |> Wallet.userInfo

        replyIds =
            model.replyIds
                |> Dict.get core.key
                |> unwrap [] Set.toList

        replies =
            replyIds
                |> List.filterMap
                    (\id ->
                        Dict.get id model.replyPosts
                    )

        sortedReplies =
            replies
                |> List.sortBy
                    (.core
                        >> Misc.sortPostsFunc
                            model.sortType
                            model.blockTimes
                            model.accounting
                            model.now
                    )
    in
    [ viewRepliesHeader model.accounting replies
    , sortedReplies
        |> List.map
            (\reply ->
                View.Post.view
                    model.dProfile
                    (model.blockTimes
                        |> Dict.get reply.core.id.block
                    )
                    model.now
                    model.replyIds
                    model.accounting
                    model.maybeBurnOrTipUX
                    model.maybeActiveTooltip
                    Nothing
                    userInfo
                    reply.core
            )
        |> column
            [ width fill
            , spacing 10
            ]
    ]
        |> column
            [ spacing 20
            , width fill
            , Element.paddingEach
                { left = 50
                , right = 0
                , top = 0
                , bottom = 0
                }
            ]


viewBottomBar : Model -> Core -> Element Msg
viewBottomBar model core =
    let
        maybeUserInfo =
            model.wallet
                |> Wallet.userInfo
    in
    maybeUserInfo
        |> unwrap
            (Input.button
                [ Background.color Theme.orange
                , padding 10
                , roundBorder
                , hover
                , Font.color black
                , Element.alignRight
                ]
                { onPress =
                    if model.wallet == NoneDetected then
                        Just ComposeOpen

                    else
                        Just ConnectToWeb3
                , label =
                    if model.wallet == Connecting then
                        View.Common.spinner 20 black

                    else
                        [ View.Img.replyArrow 15 black
                        , text "Connect wallet to reply"
                        ]
                            |> row [ spacing 10, Font.size 20 ]
                }
            )
            (\userInfo ->
                if model.compose.reply then
                    viewReplyInput model.chainSwitchInProgress model.dProfile model.compose userInfo

                else
                    [ Input.button
                        [ Background.color Theme.green
                        , padding 10
                        , roundBorder
                        , hover
                        , Font.color black
                        ]
                        { onPress = Just <| SharePost core
                        , label =
                            [ View.Img.link 15 black
                            , text "Share"
                            ]
                                |> row [ spacing 10, Font.size 20 ]
                        }
                        |> when model.shareEnabled
                    , [ Input.button
                            [ Background.color Theme.orange
                            , padding 10
                            , roundBorder
                            , hover
                            , Font.color black
                            , Element.alignBottom
                            ]
                            { onPress = Just <| ReplyOpen core.id
                            , label =
                                [ View.Img.replyArrow 15 black
                                , text "Reply"
                                ]
                                    |> row [ spacing 10, Font.size 20 ]
                            }
                      , View.Post.viewBurnOrTip core maybeUserInfo model.maybeBurnOrTipUX
                      ]
                        |> row [ spacing 10, Element.alignRight ]
                    ]
                        |> row [ width fill, Element.spaceEvenly ]
            )


viewBreadcrumbs : LogPost -> Dict PostKey RootPost -> Dict PostKey ReplyPost -> Element Msg
viewBreadcrumbs log rootPosts replyPosts =
    let
        newBreadcrumb linkTarget label =
            Input.button
                [ padding 10
                , whiteGlowAttributeSmall
                , Background.color black
                , hover
                , Font.color white
                , View.Attrs.title label
                ]
                { onPress = Just <| GotoView linkTarget
                , label =
                    View.Common.ellipsisText 20 label
                        |> el [ width <| px 90 ]
                }

        walk curr acc =
            let
                label =
                    case curr of
                        LogReply p ->
                            p.core.txHash
                                |> Eth.Utils.txHashToString

                        LogRoot p ->
                            "#" ++ p.topic

                linkTarget =
                    case curr of
                        LogReply p ->
                            ViewPost p.parent

                        LogRoot p ->
                            ViewTopic p.topic

                newElement =
                    newBreadcrumb linkTarget label

                newAcc =
                    newElement :: acc

                parentId =
                    case curr of
                        LogReply p ->
                            Just p.parent

                        LogRoot _ ->
                            Nothing
            in
            parentId
                |> Maybe.andThen
                    (\parent ->
                        Misc.getPostOrReply parent rootPosts replyPosts
                    )
                |> unwrap newAcc
                    (\val -> walk val newAcc)
    in
    walk log [ viewCurrentBreadcrumb log ]
        |> List.intersperse (el [ Font.color white, Font.bold ] <| text "/")
        |> Element.wrappedRow [ spacing 10 ]


viewCurrentBreadcrumb : LogPost -> Element Msg
viewCurrentBreadcrumb curr =
    let
        label =
            Misc.getCore curr
                |> .txHash
                |> Eth.Utils.txHashToString
    in
    View.Common.ellipsisText 20 label
        |> el [ width <| px 90 ]
        |> el
            [ padding 10
            , whiteGlowAttributeSmall
            , Background.color orange
            , View.Attrs.title label
            ]


viewReplyInput : Bool -> DisplayProfile -> ComposeModel -> UserInfo -> Element Msg
viewReplyInput chainSwitchInProgress dProfile compose userInfo =
    let
        isMobile =
            dProfile == Mobile

        submitEnabled =
            not (String.isEmpty compose.body)
                && validTopic
                && not compose.inProgress

        validTopic =
            True

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
                , Font.color black
                    |> whenAttr active
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
      , [ [ View.Img.replyArrow 25 orange
          , "Reply with"
                |> text
                |> el [ Font.color orange ]
          ]
            |> row [ spacing 10 ]
        , View.Common.chain userInfo.chain
            |> el
                [ Background.color white
                , View.Attrs.roundBorder
                , padding 5
                , Font.color black
                ]
        ]
            |> row [ spacing 10, Element.moveUp 5 ]
            |> when (not isMobile)
      ]
        |> row [ width fill, Element.spaceEvenly ]
    , [ View.Compose.viewInstructions chainSwitchInProgress dProfile userInfo
      , viewMarkdown dProfile compose
            |> el
                [ width fill
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
                                ]
                            >> el
                                [ padding 10
                                , Element.alignBottom
                                , Element.alignRight
                                ]
                        )
                    |> Element.inFront
                ]
      , [ viewBurnAmountUX compose.dollar
        , [ View.Common.cancel ComposeClose
                |> el [ Font.color black ]
          , Input.button
                [ Background.color Theme.darkGreen
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
            |> row [ Element.alignRight, spacing 20 ]
        ]
            |> row [ width fill ]
      ]
        |> column
            [ width fill
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


viewMarkdown : DisplayProfile -> ComposeModel -> Element Msg
viewMarkdown dProfile compose =
    if compose.preview then
        (if String.isEmpty compose.body then
            text "Nothing to preview"

         else
            compose.body
                |> View.Markdown.renderString dProfile
        )
            |> el
                [ width fill
                , height <| px 200
                , Element.scrollbarY
                , Font.color white
                , padding 10
                , Background.color black
                ]
            |> List.singleton
            |> column [ width fill, height fill ]

    else
        Input.multiline
            [ width fill
            , height <| px 200
            , Element.scrollbarY
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

                --, Element.scrollbarY
                , height fill
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


viewAccounting : DisplayProfile -> Accounting -> Element Msg
viewAccounting _ accounting =
    [ viewAmount Theme.darkRed accounting.totalBurned
    , viewAmount Theme.darkGreen accounting.totalTipped
    ]
        |> column
            [ spacing 5
            , height fill
            ]


viewAmount : Color -> TokenValue.TokenValue -> Element Msg
viewAmount color amount =
    [ View.Img.dollar 22 white
    , Misc.formatDollar amount
        |> text
    ]
        |> row
            [ padding 3
            , Border.rounded 3
            , Font.size 22
            , Font.color white
            , Background.color color
            , width fill
            , height fill
            ]
