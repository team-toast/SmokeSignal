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
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Helpers.Time as TimeHelpers
import Home.View
import Html.Attributes
import Json.Decode
import List.Extra
import Maybe.Extra
import Phace
import Post exposing (Post)
import Routing exposing (Route)
import Theme exposing (defaultTheme)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet


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
    in
    Element.column
        ([ Element.width Element.fill
         , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
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
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
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
                            model.posts

                Compose topic ->
                    Element.map ComposeUXMsg <|
                        ComposeUX.view
                            model.dProfile
                            walletUXPhaceInfo
                            model.composeUXModel
                            (ComposeUX.ComposingForTopic topic)

                ViewPost postId ->
                    case getPostFromId model.posts postId of
                        Just post ->
                            viewPostAndReplies
                                model.posts
                                model.blockTimes
                                model.replies
                                model.showAddressId
                                post

                        Nothing ->
                            appStatusMessage
                                defaultTheme.appStatusTextColor
                                "Loading post..."

                ViewTopic topic ->
                    Element.Lazy.lazy5
                        viewPostsForTopic
                        model.posts
                        model.blockTimes
                        model.replies
                        model.showAddressId
                        topic
        , if model.showHalfComposeUX then
            let
                maybeComposeContext =
                    case model.mode of
                        BlankMode ->
                            Nothing

                        Home _ ->
                            Nothing

                        Compose topic ->
                            Just <|
                                ComposeUX.ComposingForTopic topic

                        ViewPost replyTo ->
                            getPostFromId model.posts replyTo
                                |> Maybe.map
                                    (\post ->
                                        ComposeUX.ComposingReply
                                            replyTo
                                            (Post.getTopic post)
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


header : EH.DisplayProfile -> Mode -> WalletUXPhaceInfo -> Element Msg
header dProfile mode walletUXPhaceInfo =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color defaultTheme.headerBackground
        , Element.height <| Element.px 130
        ]
        [ Element.el
            [ Element.width <| Element.fillPortion 1
            , Element.padding 10
            ]
            EH.forgedByFoundry
        , Element.el
            [ Element.width <| Element.fillPortion 3
            ]
          <|
            Element.el [ Element.centerX ] logoBlock
        , Element.el
            [ Element.width <| Element.fillPortion 1
            ]
          <|
            case mode of
                Home _ ->
                    Element.none

                _ ->
                    Element.el
                        [ Element.alignRight
                        , Element.alignTop
                        ]
                    <|
                        Element.map MsgUp <|
                            walletUX dProfile walletUXPhaceInfo
        ]


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


viewPostAndReplies : Dict Int (List Post) -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> Post -> Element Msg
viewPostAndReplies allPosts blockTimes replies showAddressId post =
    let
        replyingPosts =
            let
                postIds =
                    replies
                        |> List.filterMap
                            (\reply ->
                                if reply.to == post.postId then
                                    Just reply.from

                                else
                                    Nothing
                            )
            in
            postIds
                |> List.map (getPostFromId allPosts)
                |> Maybe.Extra.values
                |> Dict.Extra.groupBy (.postId >> .block)
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 40
        ]
        [ viewEntirePost
            { showReplyTo = True
            , showTopic = True
            }
            (case showAddressId of
                Just (PhaceForPostAuthor postId) ->
                    postId == post.postId

                _ ->
                    False
            )
            Nothing
            post
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 20
            , Element.paddingEach
                { left = 40
                , right = 0
                , top = 0
                , bottom = 0
                }
            ]
            [ Element.el
                [ Element.Font.size 40
                , Element.Font.bold
                ]
                (Element.text "Replies")
            , viewPostsGroupedByBlock
                { showReplyTo = False
                , showTopic = False
                }
                blockTimes
                replies
                showAddressId
                replyingPosts
            ]
        ]


viewPostsForTopic : Dict Int (List Post) -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> String -> Element Msg
viewPostsForTopic allPosts blockTimes replies showAddressId topic =
    let
        filteredPosts =
            allPosts
                |> filterBlockPosts
                    (\post ->
                        Post.getTopic post == topic
                    )
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 20
        ]
    <|
        if Dict.isEmpty filteredPosts then
            appStatusMessage defaultTheme.appStatusTextColor <| "No posts found with topic '" ++ topic ++ "'"

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


viewPostsGroupedByBlock : ViewContext -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> Dict Int (List Post) -> Element Msg
viewPostsGroupedByBlock viewContext blockTimes replies showAddressId posts =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        (posts
            |> Dict.toList
            |> List.reverse
            |> List.map (viewBlocknumAndPosts viewContext blockTimes replies showAddressId)
        )


viewBlocknumAndPosts : ViewContext -> Dict Int Time.Posix -> List Reply -> Maybe PhaceIconId -> ( Int, List Post ) -> Element Msg
viewBlocknumAndPosts viewContext blockTimes replies showAddressId ( blocknum, posts ) =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing 5
            , Element.Font.italic
            , Element.Font.size 14

            -- , Element.Font.color EH.white
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.spacing 5
                ]
                [ Element.text <| "block " ++ String.fromInt blocknum
                , Element.el
                    [ Element.width Element.fill
                    , Element.height <| Element.px 1

                    -- , Element.Border.color EH.white
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
        , viewPosts viewContext replies showAddressId posts
        ]


viewPosts : ViewContext -> List Reply -> Maybe PhaceIconId -> List Post -> Element Msg
viewPosts viewContext replies showAddressId posts =
    Element.column
        [ Element.paddingXY 20 0
        , Element.spacing 20
        ]
    <|
        List.map
            (\post ->
                viewEntirePost
                    viewContext
                    (case showAddressId of
                        Just (PhaceForPostAuthor postId) ->
                            post.postId == postId

                        _ ->
                            False
                    )
                    (Just
                        (replies
                            |> List.Extra.count
                                (.to >> (==) post.postId)
                        )
                    )
                    post
            )
            posts


viewEntirePost : ViewContext -> Bool -> Maybe Int -> Post -> Element Msg
viewEntirePost viewContext showAddress numReplies post =
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
                    (PhaceForPostAuthor post.postId)
                    post.from
                    showAddress
        , Element.column
            [ Element.width Element.fill
            , Element.alignTop
            ]
            [ Element.row
                [ Element.width Element.fill ]
                [ viewDaiBurned post.burnAmount
                , viewPermalink post.postId
                ]
            , viewMainPostBlock viewContext post
            , viewNumRepliesIfNonzero
                post.postId
                (numReplies |> Maybe.withDefault 0)
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


viewMainPostBlock : ViewContext -> Post -> Element Msg
viewMainPostBlock viewContext post =
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
        , messageActions post
        ]


metadataStuff : ViewContext -> Result Json.Decode.Error Post.Metadata -> Element Msg
metadataStuff context metadataResult =
    case metadataResult of
        Err jsonDecodeErr ->
            viewMetadataDecodeError jsonDecodeErr

        Ok metadata ->
            Element.row
                [ Element.width Element.fill
                , Element.spacing 20
                ]
                ([ if context.showReplyTo then
                    maybeViewReplyInfo metadata.replyTo Nothing

                   else
                    Nothing
                 , if context.showTopic then
                    Just <|
                        Element.el [ Element.alignRight ] <|
                            viewTopic metadata.topic

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
        [ Element.text "Message topic:"
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


messageActions : Post -> Element Msg
messageActions post =
    Element.row
        [ Element.alignRight ]
        [ replyButton post.postId ]


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
