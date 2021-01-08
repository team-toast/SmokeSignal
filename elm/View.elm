module View exposing (..)

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
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Home.View
import Html.Attributes
import Json.Decode
import List.Extra
import Maybe.Extra
import MaybeDebugLog exposing (maybeDebugLog)
import Phace
import Post exposing (Post)
import PostUX.Types as PostUX
import PostUX.View as PostUX
import Routing exposing (Route)
import Theme exposing (theme)
import Time
import TokenValue exposing (TokenValue)
import Tuple3
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet exposing (Wallet)


root :
    Model
    -> Browser.Document Msg
root model =
    { title = getTitle model
    , body =
        [ Element.layout
            ([ Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
             , Element.Events.onClick ClickHappened
             , Element.height Element.fill
             ]
             -- ++ List.map Element.inFront (modals model)
            )
          <|
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.clip
                ]
                [ header model.wallet model.searchInput
                , body model
                , footer
                ]
        ]
    }


header :
    Wallet
    -> String
    -> Element Msg
header wallet searchInput =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 80
        , Element.Background.color EH.black
        , EH.moveToFront
        , whiteGlowAttribute
        , Element.paddingXY 0 15
        ]
    <|
        Element.row
            [ Element.paddingXY 100 0
            , Element.Font.color Theme.orange
            ]
            [ Element.image
                [ Element.height <| Element.px 50
                ]
                { src = "img/smokesignal-logo-horizontal.svg"
                , description = "smokesignal logo"
                }
            , Element.row
                [ Element.height Element.fill
                , Element.width <| Element.px 0
                , Element.Border.glow
                    (Element.rgba 1 1 1 0.4)
                    40
                , Element.moveLeft 50
                ]
                []
            ]


footer : Element Msg
footer =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 80
        , Element.Background.color EH.black
        , Element.alignBottom
        , EH.moveToFront
        , whiteGlowAttribute
        , Element.paddingXY 0 15
        ]
    <|
        Element.row
            [ Element.spacingXY 150 0
            , Element.Font.color Theme.orange
            , Element.centerX
            ]
            [ Element.image
                [ Element.height <| Element.px 50 ]
                { src = "img/forged-by-foundry-white.svg"
                , description =
                    "forged by foundry"
                }
            , Element.el [] <| Element.text "ABOUT"
            , Element.text "NEWS"
            , Element.text "STATS"
            , Element.image
                [ Element.height <| Element.px 50
                ]
                { src = "img/smokesignal-logo-horizontal.svg"
                , description = "smokesignal logo"
                }
            ]


body :
    Model
    -> Element Msg
body model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront <| bodyContent model
        , Element.clipY
        ]
    <|
        Element.image
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            { src = "img/smoke-bg.jpg"
            , description = "background"
            }


bodyContent : Model -> Element Msg
bodyContent model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        ]
        [ case model.mode of
            BlankMode ->
                Element.none

            Home homeModel ->
                Element.map HomeMsg <|
                    Element.Lazy.lazy
                        (\publishedPosts ->
                            Home.View.view
                                model.dProfile
                                model.donateChecked
                                model.blockTimes
                                model.now
                                model.showAddressId
                                model.demoPhaceSrc
                                model.wallet
                                publishedPosts
                                homeModel
                        )
                        model.publishedPosts

            Compose ->
                Element.map ComposeUXMsg <|
                    ComposeUX.viewFull
                        model.dProfile
                        model.donateChecked
                        model.wallet
                        model.showAddressId
                        model.composeUXModel

            ViewContext context ->
                case context of
                    Post postId ->
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
                                    [ viewPostHeader model.dProfile post
                                    , Element.Lazy.lazy5
                                        (viewPostAndReplies model.dProfile model.donateChecked model.wallet)
                                        model.publishedPosts
                                        model.blockTimes
                                        model.replies
                                        post
                                        model.postUX
                                    ]

                            Nothing ->
                                appStatusMessage
                                    theme.appStatusTextColor
                                    "Loading post..."

                    Topic topic ->
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
                                (viewPostsForTopic model.dProfile model.donateChecked model.wallet)
                                model.publishedPosts
                                model.blockTimes
                                model.replies
                                model.postUX
                                topic
                            ]
        ]


dummyElement =
    Element.none


viewPostAndReplies : DisplayProfile -> Bool -> Wallet -> PublishedPostsDict -> Dict Int Time.Posix -> List Reply -> Post.Published -> Maybe ( PostUXId, PostUX.Model ) -> Element Msg
viewPostAndReplies dProfile donateChecked wallet allPosts blockTimes replies publishedPost postUX =
    dummyElement


viewTopicHeader dProfile maybeUserInfo topic =
    dummyElement


viewPostsForTopic dprofile donateChecked wallet allPosts blockTimes replies postUX topic =
    dummyElement


viewPostHeader dProfile post =
    dummyElement


modals :
    Model
    -> List (Element Msg)
modals model =
    Maybe.Extra.values
        ([ if model.mode /= Compose && model.showHalfComposeUX then
            Just <|
                viewHalfComposeUX model

           else
            Nothing
         , Maybe.map
            (Element.el
                [ Element.alignTop
                , Element.alignRight
                , Element.padding (responsiveVal model.dProfile 20 10)
                , EH.visibility False
                ]
                << Element.el
                    [ EH.visibility True ]
            )
            (maybeTxTracker
                model.dProfile
                model.showExpandedTrackedTxs
                model.trackedTxs
            )
         , let
            showDraftInProgressButton =
                case model.mode of
                    Compose ->
                        False

                    _ ->
                        (model.showHalfComposeUX == False)
                            && (not <| Post.contentIsEmpty model.composeUXModel.content)
           in
           if showDraftInProgressButton then
            Just <|
                theme.secondaryActionButton
                    model.dProfile
                    [ Element.alignBottom
                    , Element.alignLeft
                    , Element.paddingXY 20 10
                    , Element.Border.glow
                        (Element.rgba 0 0 0 0.5)
                        5
                    ]
                    [ "Draft in Progress" ]
                    (EH.Action <| MsgUp <| StartInlineCompose model.composeUXModel.context)

           else
            Nothing
         , maybeViewDraftModal model
         , if not model.cookieConsentGranted then
            Just <| viewCookieConsentModal model.dProfile

           else
            Nothing
         ]
            ++ List.map Just
                (userNoticeEls
                    model.dProfile
                    model.userNotices
                )
        )


viewHalfComposeUX =
    Debug.todo ""


maybeViewDraftModal =
    Debug.todo ""


viewCookieConsentModal =
    Debug.todo ""


maybeTxTracker : DisplayProfile -> Bool -> List TrackedTx -> Maybe (Element Msg)
maybeTxTracker dProfile showExpanded trackedTxs =
    if List.isEmpty trackedTxs then
        Nothing

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
            Nothing

        else
            Just <|
                Element.el
                    [ Element.below <|
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
                        [ Element.Border.rounded 5
                        , Element.Background.color <| Element.rgb 0.2 0.2 0.2
                        , Element.padding (responsiveVal dProfile 10 5)
                        , Element.spacing (responsiveVal dProfile 10 5)
                        , Element.Font.size (responsiveVal dProfile 20 12)
                        , Element.pointer
                        , EH.onClickNoPropagation <|
                            if showExpanded then
                                ShowExpandedTrackedTxs False

                            else
                                ShowExpandedTrackedTxs True
                        ]
                        (renderedTallyEls
                            |> List.map (Maybe.withDefault Element.none)
                        )


trackedTxsColumn :
    List TrackedTx
    -> Element Msg
trackedTxsColumn trackedTxs =
    Element.column
        [ Element.Background.color <| Theme.lightBlue
        , Element.Border.rounded 3
        , Element.Border.glow
            (Element.rgba 0 0 0 0.2)
            4
        , Element.padding 10
        , Element.spacing 5
        , EH.onClickNoPropagation <| MsgUp NoOp
        , Element.height (Element.shrink |> Element.maximum 400)
        , Element.scrollbarY
        , Element.alignRight
        ]
        (List.map viewTrackedTxRow trackedTxs)


viewTrackedTxRow :
    TrackedTx
    -> Element Msg
viewTrackedTxRow trackedTx =
    let
        etherscanLink label =
            Element.newTabLink
                [ Element.Font.italic
                , Element.Font.color theme.linkTextColor
                ]
                { url = EthHelpers.etherscanTxUrl trackedTx.txHash
                , label = Element.text label
                }

        titleEl =
            case ( trackedTx.txInfo, trackedTx.status ) of
                ( UnlockTx, _ ) ->
                    Element.text "Unlock DAI"

                ( TipTx postId amount, _ ) ->
                    Element.row
                        []
                        [ Element.text "Tip "
                        , Element.el
                            [ Element.Font.color theme.linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                MsgUp <|
                                    GotoRoute <|
                                        Routing.ViewContext <|
                                            Post postId
                            ]
                            (Element.text "Post")
                        ]

                ( BurnTx postId amount, _ ) ->
                    Element.row
                        []
                        [ Element.text "Burn for "
                        , Element.el
                            [ Element.Font.color theme.linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                MsgUp <|
                                    GotoRoute <|
                                        Routing.ViewContext <|
                                            Post postId
                            ]
                            (Element.text "Post")
                        ]

                ( PostTx _, Mined _ ) ->
                    Element.text "Post"

                ( PostTx draft, _ ) ->
                    Element.row
                        [ Element.spacing 8
                        ]
                        [ Element.text "Post"
                        , Element.el
                            [ Element.Font.color theme.linkTextColor
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
                        PostTx draft ->
                            case maybePostId of
                                Just postId ->
                                    Element.el
                                        [ Element.Font.color theme.linkTextColor
                                        , Element.pointer
                                        , Element.Events.onClick <| MsgUp <| GotoRoute <| Routing.ViewContext <| Post postId
                                        ]
                                        (Element.text "Published")

                                Nothing ->
                                    etherscanLink "Mined"

                        _ ->
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


trackedTxStatusToColor :
    TxStatus
    -> Element.Color
trackedTxStatusToColor txStatus =
    case txStatus of
        Mining ->
            Theme.darkYellow

        Mined _ ->
            Theme.green

        Failed _ ->
            Theme.softRed


userNoticeEls :
    EH.DisplayProfile
    -> List UserNotice
    -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft (EH.responsiveVal dProfile 20 5)
            , Element.moveUp (EH.responsiveVal dProfile 20 5)
            , Element.spacing (EH.responsiveVal dProfile 10 5)
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px (EH.responsiveVal dProfile 300 150)
            , Element.Font.size (EH.responsiveVal dProfile 15 10)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight (EH.responsiveVal dProfile 20 5)
            , Element.moveDown 100
            , Element.spacing (EH.responsiveVal dProfile 10 5)
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px (EH.responsiveVal dProfile 300 150)
            , Element.Font.size (EH.responsiveVal dProfile 15 10)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice :
    EH.DisplayProfile
    -> ( Int, UserNotice )
    -> Element Msg
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
            EH.closeButton
                [ Element.alignRight
                , Element.alignTop
                , Element.moveUp 2
                ]
                EH.black
                (DismissNotice id)
    in
    Element.el
        [ Element.Background.color color
        , Element.Border.rounded (EH.responsiveVal dProfile 10 5)
        , Element.padding (EH.responsiveVal dProfile 8 3)
        , Element.width Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow
        , EH.onClickNoPropagation <| MsgUp NoOp
        ]
        (notice.mainParagraphs
            |> List.map (List.map (Element.map never))
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
