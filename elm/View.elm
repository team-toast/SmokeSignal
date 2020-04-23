module View exposing (root)

import Browser
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import ComposeUX.Types as ComposeUX
import ComposeUX.View as ComposeUX
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Lazy
import ElementMarkdown
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.Element as EH exposing (changeForMobile)
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Home.View
import Html.Attributes
import Json.Decode
import List.Extra
import Maybe.Extra
import Phace
import Post exposing (Post, PublishedPost)
import Routing exposing (Route)
import Theme exposing (defaultTheme)
import Time
import TokenValue exposing (TokenValue)
import Tuple3
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet


maxContentColWidth =
    1000


root : Model -> Browser.Document Msg
root model =
    { title = "SmokeSignal"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
            , Element.Events.onClick ClickHappened
            ]
          <|
            body model
        ]
    }


body : Model -> Element Msg
body model =
    let
        walletUXPhaceInfo =
            makeWalletUXPhaceInfo
                (Wallet.userInfo model.wallet)
                model.showAddressId
                model.demoPhaceSrc

        showDraftInProgressButton =
            case model.mode of
                Compose _ ->
                    False

                _ ->
                    (model.showHalfComposeUX == False)
                        && (model.composeUXModel.message /= "")
    in
    Element.column
        ([ Element.width Element.fill
         , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
         , Element.inFront <|
            if showDraftInProgressButton then
                defaultTheme.secondaryActionButton
                    model.dProfile
                    [ Element.alignBottom
                    , Element.alignLeft
                    , Element.paddingXY 20 10
                    , Element.Border.glow
                        (Element.rgba 0 0 0 0.5)
                        5
                    ]
                    [ "Draft in Progress" ]
                    (MsgUp <| ShowHalfComposeUX True)

            else
                Element.none
         , Element.inFront <|
            case model.draftModal of
                Just draft ->
                    Element.column
                        [ Element.centerX
                        , Element.centerY
                        , Element.Background.color defaultTheme.draftModalBackground
                        , Element.padding 20
                        , Element.spacing 10
                        , Element.Border.rounded 10
                        , Element.Border.glow
                            (Element.rgba 0 0 0 0.3)
                            10
                        , EH.onClickNoPropagation NoOp
                        ]
                        [ viewEntirePost
                            (ViewContext True True)
                            (model.showAddressId == Just PhaceForDraft)
                            Nothing
                            PhaceForDraft
                            draft.post
                        , defaultTheme.secondaryActionButton
                            model.dProfile
                            []
                            [ "Restore Draft" ]
                            (RestoreDraft draft)
                        ]

                Nothing ->
                    Element.none
         ]
            ++ List.map
                Element.inFront
                (userNoticeEls
                    model.dProfile
                    model.userNotices
                )
        )
        [ header
            model.dProfile
            model.mode
            walletUXPhaceInfo
            model.trackedTxs
            model.showExpandedTrackedTxs
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Background.color defaultTheme.appBackground
            , Element.scrollbarY
            ]
          <|
            case model.mode of
                BlankMode ->
                    Element.none

                Home homeModel ->
                    Element.map HomeMsg <|
                        Element.Lazy.lazy
                            (Home.View.view
                                model.dProfile
                                homeModel
                                walletUXPhaceInfo
                            )
                            model.publishedPosts

                Compose topic ->
                    Element.map ComposeUXMsg <|
                        ComposeUX.viewFull
                            model.dProfile
                            walletUXPhaceInfo
                            model.composeUXModel
                            (ComposeUX.ComposingForTopic topic)

                ViewPost postId ->
                    case getPublishedPostFromId model.publishedPosts postId of
                        Just post ->
                            Element.column
                                [ Element.width (Element.fill |> Element.maximum (maxContentColWidth + 100))
                                , Element.centerX
                                , Element.spacing 20
                                , Element.paddingEach
                                    { top = 20
                                    , bottom = 0
                                    , right = 0
                                    , left = 0
                                    }
                                ]
                                [ viewPostHeader post
                                , Element.Lazy.lazy5
                                    viewPostAndReplies
                                    model.publishedPosts
                                    model.blockTimes
                                    model.replies
                                    model.showAddressId
                                    post
                                ]

                        Nothing ->
                            appStatusMessage
                                defaultTheme.appStatusTextColor
                                "Loading post..."

                ViewTopic topic ->
                    Element.column
                        [ Element.width (Element.fill |> Element.maximum (maxContentColWidth + 100))
                        , Element.centerX
                        , Element.spacing 20
                        , Element.paddingEach
                            { top = 20
                            , bottom = 0
                            , right = 0
                            , left = 0
                            }
                        ]
                        [ viewTopicHeader model.dProfile (Wallet.userInfo model.wallet) topic
                        , Element.Lazy.lazy5
                            viewPostsForTopic
                            model.publishedPosts
                            model.blockTimes
                            model.replies
                            model.showAddressId
                            topic
                        ]
        , if model.showHalfComposeUX then
            let
                maybeComposeContext =
                    case model.replyTo of
                        Just replyTo ->
                            getPublishedPostFromId model.publishedPosts replyTo
                                |> Maybe.map
                                    (\minedPost ->
                                        ComposeUX.ComposingReply
                                            replyTo
                                            minedPost.post.metadata.topic
                                    )

                        Nothing ->
                            case model.mode of
                                BlankMode ->
                                    Nothing

                                Home _ ->
                                    Nothing

                                Compose topic ->
                                    Just <|
                                        ComposeUX.ComposingForTopic topic

                                ViewPost replyTo ->
                                    getPublishedPostFromId model.publishedPosts replyTo
                                        |> Maybe.map
                                            (\publishedPost ->
                                                ComposeUX.ComposingReply
                                                    replyTo
                                                    publishedPost.post.metadata.topic
                                            )

                                ViewTopic topic ->
                                    Just <|
                                        ComposeUX.ComposingForTopic topic
            in
            case maybeComposeContext of
                Just composeContext ->
                    Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.mapAttribute MsgUp <|
                            Element.above <|
                                Element.el
                                    [ Element.alignLeft
                                    ]
                                <|
                                    defaultTheme.secondaryActionButton
                                        EH.Mobile
                                        []
                                        [ "Hide" ]
                                        (ShowHalfComposeUX False)
                        ]
                        (Element.map ComposeUXMsg <|
                            ComposeUX.view
                                model.dProfile
                                walletUXPhaceInfo
                                model.composeUXModel
                                composeContext
                        )

                Nothing ->
                    Element.none

          else
            Element.none
        ]


header : EH.DisplayProfile -> Mode -> WalletUXPhaceInfo -> List TrackedTx -> Bool -> Element Msg
header dProfile mode walletUXPhaceInfo trackedTxs showExpandedTrackedTxs =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color (defaultTheme.headerBackground |> changeForMobile EH.white dProfile)
        , Element.height <| Element.px 150
        , Element.spacing 30
        , Element.padding 20
        , EH.moveToFront
        , Element.Border.glow
            (EH.black |> EH.withAlpha 0.5)
            5
        ]
        [ logoBlock
        , Element.el [ Element.centerY ] <|
            EH.forgedByFoundry
        , Element.row
            [ Element.alignRight
            , Element.spacing 10
            ]
            [ maybeTxTracker showExpandedTrackedTxs trackedTxs
            , case mode of
                Home _ ->
                    Element.none

                _ ->
                    Element.el
                        [ Element.alignRight
                        , Element.alignTop
                        ]
                    <|
                        Element.map MsgUp <|
                            walletUX dProfile False walletUXPhaceInfo
            ]
        ]



-- maybeModeEl : EH.DisplayProfile -> Mode -> WalletUXPhaceInfo -> Element Msg
-- maybeModeEl dProfile mode walletUXPhaceInfo =
--     let
--         rowContainer =
--             Element.row
--                 [ Element.spacing 40
--                 , Element.padding 20
--                 , Element.Font.size 50
--                 , Element.Font.color defaultTheme.headerTextColor
--                 , Element.centerX
--                 ]
--         buttonAttributes =
--             [ Element.paddingXY 30 10
--             , Element.centerX
--             ]
--         connectButton =
--             defaultTheme.emphasizedActionButton
--                 dProfile
--                 buttonAttributes
--                 [ "Activate Wallet to Post" ]
--                 (MsgUp <| ConnectToWeb3)
--     in
--     case mode of
--         BlankMode ->
--             Element.none
--         Home _ ->
--             Element.none
--         Compose topic ->
--             rowContainer
--                 [ Element.row []
--                     [ Element.text "Composing Post in "
--                     , Element.el
--                         [ Element.Font.italic
--                         , Element.Font.bold
--                         , Element.Font.color defaultTheme.linkTextColor
--                         , Element.pointer
--                         , Element.Events.onClick <|
--                             MsgUp <|
--                                 GotoRoute <|
--                                     Routing.ViewTopic topic
--                         ]
--                       <|
--                         Element.text topic
--                     ]
--                 ]
--         ViewPost postId ->
--             rowContainer
--                 [ Element.text <|
--                     "Viewing Post "
--                         ++ shortenedHash postId.messageHash
--                 , case walletUXPhaceInfo of
--                     UserPhaceInfo ( userInfo, _ ) ->
--                         defaultTheme.secondaryActionButton
--                             dProfile
--                             buttonAttributes
--                             [ "Reply to This Post" ]
--                             (UpdateReplyTo <| Just postId)
--                     _ ->
--                         connectButton
--                 ]
--         ViewTopic topic ->
--             rowContainer
--                 [ Element.row []
--                     [ Element.text "Viewing Topic "
--                     , Element.el
--                         [ Element.Font.italic
--                         , Element.Font.bold
--                         , Element.Font.color EH.white
--                         ]
--                       <|
--                         Element.text topic
--                     ]
--                 , case walletUXPhaceInfo of
--                     UserPhaceInfo ( userInfo, _ ) ->
--                         defaultTheme.secondaryActionButton
--                             dProfile
--                             buttonAttributes
--                             [ "Post in Topic" ]
--                             (MsgUp <| GotoRoute <| Routing.Compose topic)
--                     _ ->
--                         connectButton
--                 ]


logoBlock : Element Msg
logoBlock =
    Element.column
        [ Element.spacing 15 ]
        [ Element.row
            [ Element.spacing 15
            , Element.centerX
            ]
            [ Element.row
                [ Element.Font.size 50
                , Element.Font.bold
                , Element.pointer
                , Element.Events.onClick <| MsgUp <| GotoRoute <| Routing.Home
                ]
                [ Element.el [ Element.Font.color Theme.darkGray ] <| Element.text "Smoke"
                , Element.el [ Element.Font.color <| Element.rgb 1 0.5 0 ] <| Element.text "Signal"
                ]
            ]
        , Element.el
            [ Element.Font.size 20
            , Element.centerX
            , Element.Font.color Theme.softRed
            ]
            (Element.text "Free Speech at the Protocol Level")
        ]


maybeTxTracker : Bool -> List TrackedTx -> Element Msg
maybeTxTracker showExpanded trackedTxs =
    if List.isEmpty trackedTxs then
        Element.none

    else
        let
            tallyFunc : TrackedTx -> ( Int, Int, Int ) -> ( Int, Int, Int )
            tallyFunc trackedTx totals =
                case trackedTx.status of
                    Mining ->
                        Tuple3.mapFirst ((+) 1) totals

                    Mined _ ->
                        Tuple3.mapSecond ((+) 1) totals

                    Failed _ ->
                        Tuple3.mapThird ((+) 1) totals

            tallies =
                trackedTxs
                    |> List.foldl tallyFunc ( 0, 0, 0 )

            renderedTallyEls =
                tallies
                    |> TupleHelpers.mapTuple3
                        (\n ->
                            if n == 0 then
                                Nothing

                            else
                                Just n
                        )
                    |> TupleHelpers.mapEachTuple3
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Element.Font.color <| trackedTxStatusToColor Mining ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mining"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Element.Font.color <| trackedTxStatusToColor <| Mined Nothing ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mined"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Element.Font.color <| trackedTxStatusToColor (Failed MinedButExecutionFailed) ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs failed"
                            )
                        )
                    |> TupleHelpers.tuple3ToList
        in
        if List.all Maybe.Extra.isNothing renderedTallyEls then
            Element.none

        else
            Element.el
                [ Element.alignRight
                , Element.height Element.fill
                , Element.below <|
                    if showExpanded then
                        Element.el
                            [ Element.alignRight
                            , Element.alignTop
                            ]
                        <|
                            trackedTxsColumn trackedTxs

                    else
                        Element.none
                ]
            <|
                Element.column
                    [ Element.alignRight
                    , Element.height Element.fill
                    , Element.Border.rounded 5
                    , Element.Background.color <| Element.rgba 1 1 1 0.3
                    , Element.padding 10
                    , Element.spacing 10
                    , Element.pointer
                    , EH.onClickNoPropagation <|
                        if showExpanded then
                            ShowExpandedTrackedTxs False

                        else
                            ShowExpandedTrackedTxs True
                    ]
                    (renderedTallyEls
                        |> List.map (Maybe.withDefault Element.none)
                        |> List.map (Element.el [ Element.height Element.fill ])
                    )


trackedTxsColumn : List TrackedTx -> Element Msg
trackedTxsColumn trackedTxs =
    Element.column
        [ Element.Background.color <| Theme.lightBlue
        , Element.Border.rounded 3
        , Element.Border.glow
            (Element.rgba 0 0 0 0.2)
            4
        , Element.padding 10
        , Element.spacing 5
        , EH.onClickNoPropagation NoOp
        , Element.height (Element.shrink |> Element.maximum 400)
        , Element.scrollbarY
        , Element.alignRight
        ]
        (List.map viewTrackedTxRow trackedTxs)


viewTrackedTxRow : TrackedTx -> Element Msg
viewTrackedTxRow trackedTx =
    let
        etherscanLink label =
            Element.newTabLink
                [ Element.Font.italic
                , Element.Font.color defaultTheme.linkTextColor
                ]
                { url = EthHelpers.etherscanTxUrl trackedTx.txHash
                , label = Element.text label
                }

        titleEl =
            case ( trackedTx.txInfo, trackedTx.status ) of
                ( UnlockTx, _ ) ->
                    Element.text "Unlock DAI"

                ( PostTx _, Mined _ ) ->
                    Element.text "Post"

                ( PostTx draft, _ ) ->
                    Element.row
                        [ Element.spacing 8
                        ]
                        [ Element.text "Post"
                        , Element.el
                            [ Element.Font.color defaultTheme.linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <| ViewDraft <| Just draft
                            ]
                            (Element.text "(View Draft)")
                        ]

        statusEl =
            case trackedTx.status of
                Mining ->
                    etherscanLink "Mining"

                Failed failReason ->
                    case failReason of
                        MinedButExecutionFailed ->
                            etherscanLink "Failed"

                Mined maybePostId ->
                    case trackedTx.txInfo of
                        UnlockTx ->
                            etherscanLink "Mined"

                        PostTx draft ->
                            case maybePostId of
                                Just postId ->
                                    Element.el
                                        [ Element.Font.color defaultTheme.linkTextColor
                                        , Element.pointer
                                        , Element.Events.onClick <| MsgUp <| GotoRoute <| Routing.ViewPost postId
                                        ]
                                        (Element.text "Published")

                                Nothing ->
                                    etherscanLink "Mined"
    in
    Element.row
        [ Element.width <| Element.px 250
        , Element.Background.color
            (trackedTxStatusToColor trackedTx.status
                |> EH.withAlpha 0.3
            )
        , Element.Border.rounded 2
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.3
        , Element.padding 4
        , Element.spacing 4
        , Element.Font.size 20
        ]
        [ titleEl
        , Element.el [ Element.alignRight ] <| statusEl
        ]


trackedTxStatusToColor : TxStatus -> Element.Color
trackedTxStatusToColor txStatus =
    case txStatus of
        Mining ->
            Theme.darkYellow

        Mined _ ->
            Theme.green

        Failed _ ->
            Theme.softRed


userNoticeEls : EH.DisplayProfile -> List UserNotice -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft (20 |> EH.changeForMobile 5 dProfile)
            , Element.moveUp (20 |> EH.changeForMobile 5 dProfile)
            , Element.spacing (10 |> EH.changeForMobile 5 dProfile)
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px (300 |> EH.changeForMobile 150 dProfile)
            , Element.Font.size (15 |> EH.changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight (20 |> EH.changeForMobile 5 dProfile)
            , Element.moveDown 100
            , Element.spacing (10 |> EH.changeForMobile 5 dProfile)
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px (300 |> EH.changeForMobile 150 dProfile)
            , Element.Font.size (15 |> EH.changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice : EH.DisplayProfile -> ( Int, UserNotice ) -> Element Msg
userNotice dProfile ( id, notice ) =
    let
        color =
            case notice.noticeType of
                UN.Update ->
                    Element.rgb255 100 200 255

                UN.Caution ->
                    Element.rgb255 255 188 0

                UN.Error ->
                    Element.rgb255 255 70 70

                UN.ShouldBeImpossible ->
                    Element.rgb255 200 200 200

        textColor =
            case notice.noticeType of
                UN.Error ->
                    Element.rgb 1 1 1

                _ ->
                    Element.rgb 0 0 0

        closeElement =
            Element.el
                [ Element.alignRight
                , Element.alignTop
                , Element.moveUp 5
                , Element.moveRight 5
                ]
                (EH.closeButton True (DismissNotice id))
    in
    Element.el
        [ Element.Background.color color
        , Element.Border.rounded (10 |> EH.changeForMobile 5 dProfile)
        , Element.padding (8 |> EH.changeForMobile 3 dProfile)
        , Element.width Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow
        , EH.onClickNoPropagation NoOp
        ]
        (notice.mainParagraphs
            |> List.map (List.map mapNever)
            |> List.indexedMap
                (\pNum paragraphLines ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Element.Font.color textColor
                        , Element.spacing 1
                        ]
                        (if pNum == 0 then
                            closeElement :: paragraphLines

                         else
                            paragraphLines
                        )
                )
            |> Element.column
                [ Element.spacing 4
                , Element.width Element.fill
                ]
        )


mapNever : Element Never -> Element Msg
mapNever =
    Element.map (always NoOp)


viewPostAndReplies : PublishedPostsDict -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> PublishedPost -> Element Msg
viewPostAndReplies allPosts blockTimes replies showAddressId publishedPost =
    let
        replyingPosts =
            let
                postIds =
                    replies
                        |> List.filterMap
                            (\reply ->
                                if reply.to == publishedPost.id then
                                    Just reply.from

                                else
                                    Nothing
                            )
            in
            postIds
                |> List.map (getPublishedPostFromId allPosts)
                |> Maybe.Extra.values
                |> Dict.Extra.groupBy (.id >> .block)
    in
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum maxContentColWidth)
        , Element.height Element.fill
        , Element.spacing 40
        , Element.padding 20
        ]
        [ viewEntirePost
            { showReplyTo = True
            , showTopic = True
            }
            (case showAddressId of
                Just (PhaceForPublishedPost postId) ->
                    postId == publishedPost.id

                _ ->
                    False
            )
            Nothing
            (PhaceForPublishedPost publishedPost.id)
            publishedPost.post
        , if Dict.isEmpty replyingPosts then
            Element.none

          else
            Element.column
                [ Element.width Element.fill
                , Element.spacing 20
                ]
                [ Element.el
                    [ Element.Font.size 50
                    , Element.Font.bold
                    , Element.Font.color defaultTheme.mainTextColor
                    ]
                    (Element.text "Replies")
                , Element.el
                    [ Element.width Element.fill
                    , Element.paddingEach
                        { left = 40
                        , right = 0
                        , top = 0
                        , bottom = 0
                        }
                    ]
                  <|
                    viewPostsGroupedByBlock
                        { showReplyTo = False
                        , showTopic = False
                        }
                        blockTimes
                        replies
                        showAddressId
                        replyingPosts
                ]
        ]


viewPostHeader : PublishedPost -> Element Msg
viewPostHeader publishedPost =
    Element.row
        (subheaderAttributes
            ++ [ Element.spacing 40
               , Element.Font.center
               ]
        )
        [ Element.el [ Element.Font.bold ] <| Element.text "Viewing Post"
        , Element.column
            [ Element.Font.size 16 ]
            [ Element.text <|
                "id: "
                    ++ (publishedPost.id.messageHash |> Eth.Utils.hexToString)
            , Element.newTabLink
                [ Element.Font.color defaultTheme.linkTextColor ]
                { url = EthHelpers.etherscanTxUrl publishedPost.txHash
                , label = Element.text "View on etherscan"
                }
            ]
        ]


viewPostsForTopic : PublishedPostsDict -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> String -> Element Msg
viewPostsForTopic allPosts blockTimes replies showAddressId topic =
    let
        filteredPosts =
            allPosts
                |> filterBlockPosts
                    (\publishedPost ->
                        publishedPost.post.metadata.topic == topic
                    )
    in
    Element.column
        [ Element.width (Element.fill |> Element.maximum maxContentColWidth)
        , Element.centerX
        , Element.height Element.fill
        , Element.padding 20
        , Element.spacing 40
        ]
        [ if Dict.isEmpty filteredPosts then
            appStatusMessage defaultTheme.appStatusTextColor <| "Haven't yet found any posts for this topic..."

          else
            Element.Lazy.lazy5
                viewPostsGroupedByBlock
                { showTopic = False
                , showReplyTo = True
                }
                blockTimes
                replies
                showAddressId
                filteredPosts
        ]


viewTopicHeader : EH.DisplayProfile -> Maybe UserInfo -> String -> Element Msg
viewTopicHeader dProfile maybeUserInfo topic =
    Element.column
        (subheaderAttributes
            ++ [ Element.spacing 10 ]
        )
        [ Element.row
            []
            [ Element.el [ Element.Font.bold ] <| Element.text "Viewing Topic "
            , Element.el
                [ Element.Font.bold
                , Element.Font.italic
                ]
              <|
                Element.text topic
            ]
        , case maybeUserInfo of
            Just userInfo ->
                defaultTheme.secondaryActionButton
                    dProfile
                    []
                    [ "Post in Topic" ]
                    (MsgUp <| GotoRoute <| Routing.Compose topic)

            Nothing ->
                defaultTheme.emphasizedActionButton
                    dProfile
                    [ Element.paddingXY 30 10 ]
                    [ "Activate Wallet to Post" ]
                    (MsgUp <| ConnectToWeb3)
        ]


viewPostsGroupedByBlock : ViewContext -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> PublishedPostsDict -> Element Msg
viewPostsGroupedByBlock viewContext blockTimes replies showAddressId publishedPosts =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        (publishedPosts
            |> Dict.toList
            |> List.reverse
            |> List.map (viewBlocknumAndPosts viewContext blockTimes replies showAddressId)
        )


viewBlocknumAndPosts : ViewContext -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> ( Int, List PublishedPost ) -> Element Msg
viewBlocknumAndPosts viewContext blockTimes replies showAddressId ( blocknum, publishedPosts ) =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing 5
            , Element.Font.italic
            , Element.Font.size 14
            , Element.Font.color defaultTheme.mainTextColor
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.spacing 5
                ]
                [ Element.text <| "block " ++ String.fromInt blocknum
                , Element.el
                    [ Element.width Element.fill
                    , Element.height <| Element.px 1
                    , Element.Border.color defaultTheme.mainTextColor
                    , Element.Border.widthEach
                        { top = 1
                        , bottom = 0
                        , right = 0
                        , left = 0
                        }
                    , Element.Border.dashed
                    ]
                    Element.none
                ]
            , blockTimes
                |> Dict.get blocknum
                |> Maybe.map posixToString
                |> Maybe.withDefault "[fetching block timestamp]"
                |> Element.text
            ]
        , viewPosts viewContext replies showAddressId publishedPosts
        ]


viewPosts : ViewContext -> List Reply -> Maybe PhaceIconId -> List PublishedPost -> Element Msg
viewPosts viewContext replies showAddressId pusblishedPosts =
    Element.column
        [ Element.paddingXY 20 0
        , Element.spacing 20
        ]
    <|
        List.map
            (\publishedPost ->
                viewEntirePost
                    viewContext
                    (case showAddressId of
                        Just (PhaceForPublishedPost postId) ->
                            publishedPost.id == postId

                        _ ->
                            False
                    )
                    (Just
                        (replies
                            |> List.Extra.count
                                (.to >> (==) publishedPost.id)
                        )
                    )
                    (PhaceForPublishedPost publishedPost.id)
                    publishedPost.post
            )
            pusblishedPosts


viewEntirePost : ViewContext -> Bool -> Maybe Int -> PhaceIconId -> Post -> Element Msg
viewEntirePost viewContext showAddress maybeNumReplies phaceIconId post =
    let
        maybePostId =
            case phaceIconId of
                PhaceForPublishedPost postId ->
                    Just postId

                _ ->
                    Nothing
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.el
            [ Element.alignTop
            , Element.height <| Element.px 100
            ]
          <|
            Element.map MsgUp <|
                phaceElement
                    True
                    phaceIconId
                    post.from
                    showAddress
        , Element.column
            [ Element.width Element.fill
            , Element.alignTop
            ]
            [ Element.row
                [ Element.width Element.fill ]
                [ viewDaiBurned post.burnAmount
                , Maybe.map viewPermalink maybePostId
                    |> Maybe.withDefault Element.none
                ]
            , viewMainPostBlock viewContext maybePostId post
            , case ( maybePostId, maybeNumReplies ) of
                ( Nothing, _ ) ->
                    Element.none

                ( _, Nothing ) ->
                    Element.none

                ( Just postId, Just numReplies ) ->
                    viewNumRepliesIfNonzero
                        postId
                        numReplies
            ]
        ]


viewDaiBurned : TokenValue -> Element Msg
viewDaiBurned amount =
    Element.el
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color defaultTheme.daiBurnedBackground
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignLeft
        ]
    <|
        Element.row
            [ Element.spacing 3

            -- , Element.Font.color EH.white
            ]
            [ daiSymbol defaultTheme.daiBurnedTextIsWhite [ Element.height <| Element.px 18 ]
            , Element.text <| TokenValue.toConciseString amount
            ]


viewPermalink : Post.Id -> Element Msg
viewPermalink postId =
    Element.el
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color defaultTheme.postBodyBackground
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignRight
        ]
    <|
        Element.link
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.Font.size 16
            ]
            { url =
                Routing.routeToFullDotEthUrlString <|
                    Routing.ViewPost <|
                        postId
            , label = Element.text ".eth permalink"
            }


viewMainPostBlock : ViewContext -> Maybe Post.Id -> Post -> Element Msg
viewMainPostBlock viewContext maybePostId post =
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        , Element.spacing 20
        , Element.Background.color (Element.rgb 0.8 0.8 1)
        , Element.Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomRight = 10
            , bottomLeft = 10
            }
        , Element.alignTop
        ]
        [ metadataStuff viewContext post.metadata
        , Post.renderContentOrError defaultTheme post.message
        , Maybe.map messageActions maybePostId
            |> Maybe.withDefault Element.none
        ]


metadataStuff : ViewContext -> Post.Metadata -> Element Msg
metadataStuff context metadata =
    case metadata.maybeDecodeError of
        Just jsonDecodeErr ->
            viewMetadataDecodeError jsonDecodeErr

        Nothing ->
            Element.row
                [ Element.spacing 20
                , Element.width Element.fill
                ]
                ([ if context.showTopic then
                    Just <|
                        Element.el [ Element.alignLeft ] <|
                            viewTopic metadata.topic

                   else
                    Nothing
                 , if context.showReplyTo then
                    maybeViewReplyInfo metadata.replyTo Nothing

                   else
                    Nothing
                 ]
                    |> Maybe.Extra.values
                )


viewTopic : String -> Element Msg
viewTopic topic =
    Element.column
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        , Element.clipX
        , Element.scrollbarX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "Topic:"
        , Element.el
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                MsgUp <|
                    GotoRoute <|
                        Routing.ViewTopic topic
            ]
            (Element.text topic)
        ]


viewMetadataDecodeError : Json.Decode.Error -> Element Msg
viewMetadataDecodeError error =
    Element.el
        [ Element.Font.color defaultTheme.errorTextColor
        , Element.Font.italic
        , Element.Font.size 18
        ]
        (Element.text <|
            "Message contains malformed metadata: "
                ++ Json.Decode.errorToString error
        )


messageActions : Post.Id -> Element Msg
messageActions postId =
    Element.row
        [ Element.alignRight ]
        [ replyButton postId ]


replyButton : Post.Id -> Element Msg
replyButton postId =
    Element.el
        [ Element.padding 7
        , Element.pointer
        , Element.Border.rounded 4
        , Element.Background.color <| Element.rgba 1 1 1 0.3
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 5
            , color = Element.rgba 0 0 0 0.1
            }
        , Element.Events.onClick <| UpdateReplyTo <| Just postId
        , Element.width <| Element.px 30
        ]
    <|
        Element.image
            [ Element.width Element.fill ]
            { src = "img/reply-arrow.svg"
            , description = "reply"
            }


maybeViewReplyInfo : Maybe Post.Id -> Maybe Msg -> Maybe (Element Msg)
maybeViewReplyInfo maybePostId maybeCloseMsg =
    case maybePostId of
        Nothing ->
            Nothing

        Just postId ->
            Just <|
                Element.row
                    [ Element.padding 10
                    , Element.Border.rounded 5
                    , Element.Font.size 20
                    , Element.Font.italic
                    , Element.Background.color <| Element.rgba 1 1 1 0.5
                    , Element.spacing 5
                    ]
                    [ Element.column
                        [ Element.spacing 3
                        ]
                        [ Element.text "Replying to:"
                        , Element.el
                            [ Element.Font.color defaultTheme.linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                MsgUp <|
                                    GotoRoute <|
                                        Routing.ViewPost postId
                            ]
                            (Element.text <|
                                shortenedHash postId.messageHash
                            )
                        ]
                    , case maybeCloseMsg of
                        Just closeMsg ->
                            Element.image
                                [ Element.padding 4
                                , Element.alignTop
                                , Element.pointer
                                , Element.Events.onClick closeMsg
                                ]
                                { src = "img/remove-circle-black.svg"
                                , description = "remove"
                                }

                        Nothing ->
                            Element.none
                    ]


viewNumRepliesIfNonzero : Post.Id -> Int -> Element Msg
viewNumRepliesIfNonzero postId numReplies =
    if numReplies == 0 then
        Element.none

    else
        Element.el
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                MsgUp <|
                    GotoRoute <|
                        Routing.ViewPost <|
                            postId
            , Element.Font.italic
            , Element.paddingXY 20 10
            ]
            (Element.text <|
                String.fromInt numReplies
                    ++ (if numReplies == 1 then
                            " reply"

                        else
                            " replies"
                       )
            )
