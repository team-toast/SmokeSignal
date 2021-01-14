module View exposing (view)

--import View.Post

import Browser
import Dict exposing (Dict)
import Element exposing (Attribute, Element, column, el, fill, height, padding, paddingXY, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, responsiveVal)
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Misc exposing (getPublishedPostFromId, getTitle)
import Phace
import Theme exposing (theme)
import Time
import Tuple3
import Types exposing (Context(..), FailReason(..), Id, Metadata, Mode(..), Model, Msg(..), PublishedPostsDict, Route(..), TrackedTx, TxInfo(..), TxStatus(..), UnlockStatus(..), UserInfo, ViewContext(..), Wallet)
import UserNotice as UN exposing (UserNotice)
import View.Common exposing (daiSymbol, phaceElement, whiteGlowAttribute, whiteGlowAttributeSmall)
import View.Home
import View.Post
import Wallet


view : Model -> Browser.Document Msg
view model =
    { title = getTitle model
    , body =
        viewPage model
            |> render model
            |> List.singleton
    }


render : Model -> Element Msg -> Html Msg
render model =
    modals model
        |> List.map Element.inFront
        |> (++)
            [ Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
            , Element.Events.onClick ClickHappened
            , Element.height Element.fill
            ]
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }


viewPage : Model -> Element Msg
viewPage model =
    [ header model.wallet model.searchInput
    , body model
    , footer
    ]
        |> Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clip
            ]


header : Wallet -> String -> Element Msg
header wallet searchInput =
    [ Element.image
        [ height <| px 50
        ]
        { src = "img/smokesignal-logo-horizontal.svg"
        , description = "smokesignal logo"
        }
    , Input.text
        [ fill |> Element.maximum 350 |> width
        , Background.color black
        ]
        { onChange = always ClickHappened
        , label = Input.labelHidden ""
        , placeholder =
            "Search . . ."
                |> text
                |> Input.placeholder []
                |> Just
        , text = searchInput
        }
    , [ Element.image
            [ height <| px 50
            ]
            { src = "/img/share.svg"
            , description = ""
            }
      , Element.image
            [ height <| px 50
            ]
            { src = "/img/info.svg"
            , description = ""
            }
      , Element.image
            [ height <| px 50
            ]
            { src = "/img/foundry-icon.svg"
            , description = ""
            }
      , "Login"
            |> text
            |> el []
      , "Sign Up"
            |> text
            |> el []
      ]
        |> row [ width fill, spaceEvenly ]
    ]
        |> row
            [ Font.color Theme.orange
            , width fill
            , Element.centerY
            , spacing 50
            ]
        |> el
            [ width fill
            , paddingXY 100 0
            , height <| px 80
            , Background.color EH.black
            , whiteGlowAttribute
            , EH.moveToFront
            ]


footer : Element Msg
footer =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 80
        , Background.color EH.black
        , Element.alignBottom
        , EH.moveToFront
        , whiteGlowAttribute
        , Element.paddingXY 0 15
        ]
    <|
        Element.row
            [ Element.spacingXY 150 0
            , Font.color Theme.orange
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


body : Model -> Element Msg
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

            ModeHome ->
                View.Home.view model

            ModeCompose ->
                --Element.map ComposeUXMsg <|
                --ComposeUX.viewFull
                --model.dProfile
                --model.donateChecked
                --model.wallet
                --model.showAddressId
                --model.composeUXModel
                -- TODO
                text "compose"

            ViewContext context ->
                case context of
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
                                    [--, Element.Lazy.lazy5
                                     --(viewPostAndReplies model.dProfile model.donateChecked model.wallet)
                                     --model.publishedPosts
                                     --model.blockTimes
                                     --model.replies
                                     --post
                                     --model.postUX
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
                            --[ Element.map HomeMsg <|
                            --Home.View.banner model.dProfile
                            [ viewTopicHeader model.dProfile (Wallet.userInfo model.wallet) topic
                            , Element.Lazy.lazy4
                                (viewPostsForTopic model.dProfile model.donateChecked False model.wallet)
                                model.blockTimes
                                model.now
                                topic
                                model.publishedPosts
                            ]
        ]



--viewPostAndReplies :
--DisplayProfile
---> Bool
---> Wallet
---> PublishedPostsDict
---> Dict Int Time.Posix
---> List ReplyIds
---> Published
---> Maybe ( PostUXId, PostUX.Model )
---> Element Msg
--viewPostAndReplies dProfile donateChecked wallet allPosts blockTimes replies publishedPost postUX =
--dummyElement


viewTopicHeader :
    DisplayProfile
    -> Maybe UserInfo
    -> String
    -> Element Msg
viewTopicHeader dProfile maybeUserInfo topic =
    --Element.map TopicUXMsg <|
    --TopicUX.topicHeader
    --dProfile
    --topic
    Element.none


viewPostsForTopic :
    DisplayProfile
    -> Bool
    -> Bool
    -> Wallet
    -> Dict Int Time.Posix
    -> Time.Posix
    -> String
    -> PublishedPostsDict
    -> Element Msg
viewPostsForTopic dProfile donateChecked showAddressOnPhace wallet blockTimes now topic allPosts =
    --Element.map TopicUXMsg <|
    --TopicUX.view
    --dProfile
    --donateChecked
    --showAddressOnPhace
    --topic
    --blockTimes
    --now
    --wallet
    --Nothing
    --allPosts
    Element.none


modals : Model -> List (Element Msg)
modals model =
    Maybe.Extra.values
        ([ if model.mode /= ModeCompose && model.showHalfComposeUX then
            --Just <|
            --viewHalfComposeUX model
            Nothing

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
                    ModeCompose ->
                        False

                    _ ->
                        --(model.showHalfComposeUX == False)
                        --&& (not <| Post.contentIsEmpty model.composeUXModel.content)
                        -- TODO
                        False
           in
           if showDraftInProgressButton then
            --Just <|
            --theme.secondaryActionButton
            --model.dProfile
            --[ Element.alignBottom
            --, Element.alignLeft
            --, Element.paddingXY 20 10
            --, Border.glow
            --(Element.rgba 0 0 0 0.5)
            --5
            --]
            --[ "Draft in Progress" ]
            --(EH.Action <| StartInlineCompose model.composeUXModel.context)
            -- TODO
            Nothing

           else
            Nothing

         --, maybeViewDraftModal model
         , if not model.cookieConsentGranted then
            --Just <| viewCookieConsentModal model.dProfile
            Nothing

           else
            Nothing
         ]
            ++ List.map Just
                (userNoticeEls
                    model.dProfile
                    model.userNotices
                )
        )


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
                                    [ Font.color <| trackedTxStatusToColor Mining ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mining"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Font.color <| trackedTxStatusToColor <| Mined Nothing ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mined"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Font.color <| trackedTxStatusToColor (Failed MinedButExecutionFailed) ]
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
                        [ Border.rounded 5
                        , Background.color <| Element.rgb 0.2 0.2 0.2
                        , Element.padding (responsiveVal dProfile 10 5)
                        , Element.spacing (responsiveVal dProfile 10 5)
                        , Font.size (responsiveVal dProfile 20 12)
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
        [ Background.color <| Theme.lightBlue
        , Border.rounded 3
        , Border.glow
            (Element.rgba 0 0 0 0.2)
            4
        , Element.padding 10
        , Element.spacing 5

        --, EH.onClickNoPropagation  NoOp
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
                [ Font.italic
                , Font.color theme.linkTextColor
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
                            [ Font.color theme.linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                GotoRoute <|
                                    RouteViewContext <|
                                        Types.ViewPost postId
                            ]
                            (Element.text "Post")
                        ]

                ( BurnTx postId amount, _ ) ->
                    Element.row
                        []
                        [ Element.text "Burn for "
                        , Element.el
                            [ Font.color theme.linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                GotoRoute <|
                                    RouteViewContext <|
                                        ViewPost postId
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
                            [ Font.color theme.linkTextColor
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
                                        [ Font.color theme.linkTextColor
                                        , Element.pointer
                                        , Element.Events.onClick <| GotoRoute <| RouteViewContext <| ViewPost postId
                                        ]
                                        (Element.text "Published")

                                Nothing ->
                                    etherscanLink "Mined"

                        _ ->
                            etherscanLink "Mined"
    in
    Element.row
        [ Element.width <| Element.px 250
        , Background.color
            (trackedTxStatusToColor trackedTx.status
                |> EH.withAlpha 0.3
            )
        , Border.rounded 2
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0 0.3
        , Element.padding 4
        , Element.spacing 4
        , Font.size 20
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
            , Font.size (EH.responsiveVal dProfile 15 10)
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
            , Font.size (EH.responsiveVal dProfile 15 10)
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
        [ Background.color color
        , Border.rounded (EH.responsiveVal dProfile 10 5)
        , Element.padding (EH.responsiveVal dProfile 8 3)
        , Element.width Element.fill
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow

        --, EH.onClickNoPropagation <| MsgUp NoOp
        ]
        (notice.mainParagraphs
            |> List.map (List.map (Element.map never))
            |> List.indexedMap
                (\pNum paragraphLines ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Font.color textColor
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


shortenedHash :
    Hex
    -> String
shortenedHash hash =
    let
        hashStr =
            Eth.Utils.hexToString hash
    in
    if String.length hashStr <= 10 then
        hashStr

    else
        String.left 6 hashStr
            ++ "..."
            ++ String.right 4 hashStr


emphasizedText : String -> Element Msg
emphasizedText =
    Element.el
        [ Font.bold
        , Font.color EH.white
        ]
        << Element.text


appStatusMessage : Element.Color -> String -> Element Msg
appStatusMessage color errStr =
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.paragraph
            [ Element.centerX
            , Element.centerY
            , Font.center
            , Font.italic
            , Font.color color
            , Font.size 36
            , Element.width (Element.fill |> Element.maximum 800)
            , Element.padding 40
            ]
            [ Element.text errStr ]


posixToString : Time.Posix -> String
posixToString t =
    let
        z =
            Time.utc
    in
    String.fromInt (Time.toYear z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt <| TimeHelpers.monthToInt <| Time.toMonth z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toDay z t))
        ++ " "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour z t))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute z t))
        ++ " (UTC)"


subheaderAttributes : DisplayProfile -> List (Attribute Msg)
subheaderAttributes dProfile =
    [ Element.paddingXY 0 (responsiveVal dProfile 20 10)
    , Font.size (responsiveVal dProfile 50 30)
    , Font.color theme.headerTextColor
    ]


commonFontSize : DisplayProfile -> Int
commonFontSize dProfile =
    case dProfile of
        Desktop ->
            24

        Mobile ->
            18


viewMetadata : Bool -> Metadata -> Element Msg
viewMetadata showContext metadata =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ case metadata.maybeDecodeError of
            Just jsonDecodeErr ->
                viewMetadataDecodeError jsonDecodeErr

            Nothing ->
                Element.none
        , if showContext then
            Element.el [ Element.alignLeft ] <|
                viewContext metadata.context

          else
            Element.none
        ]


viewMetadataDecodeError : String -> Element Msg
viewMetadataDecodeError error =
    Element.el
        [ Border.rounded 5
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0 0.3
        , Element.clip
        ]
    <|
        Element.el
            [ Font.color theme.errorTextColor
            , Font.italic
            , Font.size 18
            , Element.height (Element.shrink |> Element.maximum 80)
            , Element.width (Element.shrink |> Element.maximum 400)
            , Element.scrollbars
            , Background.color <| Element.rgba 1 0 0 0.1
            ]
            (Element.text <|
                "Metadata decode error:\n\n"
                    ++ error
            )


viewContext : Context -> Element Msg
viewContext context =
    case context of
        Reply postId ->
            viewReplyInfo postId

        TopLevel topic ->
            viewTopic topic


viewTopic : String -> Element Msg
viewTopic topic =
    Element.column
        [ Element.padding 10
        , Border.rounded 5
        , Font.size 20
        , Font.italic
        , Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        , Element.clipX
        , Element.scrollbarX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "Topic:"
        , Element.el
            [ Font.color theme.linkTextColor
            , Element.pointer

            --, Element.Events.onClick <|
            --GotoRoute <|
            --RouteViewContext <|
            --Topic topic
            ]
            (Element.text topic)
        ]


viewReplyInfo : Id -> Element Msg
viewReplyInfo postId =
    Element.row
        [ Element.padding 10
        , Border.rounded 5
        , Font.size 20
        , Font.italic
        , Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        ]
        [ Element.column
            [ Element.spacing 3
            ]
            [ Element.text "Replying to:"
            , Element.el
                [ Font.color theme.linkTextColor
                , Element.pointer

                --, Element.Events.onClick <|
                --GotoRoute <|
                --RouteViewContext <|
                --Types.ViewPost postId
                ]
                (Element.text <|
                    shortenedHash postId.messageHash
                )
            ]
        ]


coloredAppTitle : List (Attribute Msg) -> Element Msg
coloredAppTitle attributes =
    Element.row attributes
        [ Element.el [ Font.color Theme.darkGray ] <| Element.text "Smoke"
        , Element.el [ Font.color <| Element.rgb 1 0.5 0 ] <| Element.text "Signal"
        ]


maxContentColWidth =
    1000
