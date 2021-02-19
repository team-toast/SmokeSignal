module View exposing (view)

import Browser
import Dict
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Helpers.Element as EH exposing (DisplayProfile(..), black, responsiveVal)
import Helpers.Eth as EthHelpers
import Helpers.Tuple as TupleHelpers
import Html exposing (Html)
import Maybe.Extra
import Misc exposing (getTitle)
import Theme exposing (theme)
import Tuple3
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import View.Attrs exposing (cappedWidth, hover, whiteGlowAttribute, whiteGlowAttributeSmall)
import View.Common exposing (appStatusMessage, whenAttr)
import View.Compose
import View.Home
import View.Modal
import View.PostPage
import View.Sidebar
import View.Topic
import View.Topics


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
            [ Element.Events.onClick ClickHappened
            , height fill
            , width fill
            , View.Attrs.typeFont
            , View.Sidebar.viewWallet model
                |> el
                    [ width fill
                    , Element.alignBottom
                    , Background.color black
                    , padding 20
                    ]
                |> Element.inFront
                |> whenAttr (model.dProfile == Mobile)
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
    let
        isDesktop =
            model.dProfile == EH.Desktop
    in
    [ header model
    , viewBody model
        |> el [ height fill, cappedWidth maxContentColWidth, Element.centerX ]
        |> el
            [ width fill
            , height fill
            , Element.scrollbarY
            , View.Modal.viewNewToSmokeSignal model.dProfile
                |> View.Common.when isDesktop
                |> Element.inFront
                |> View.Common.whenAttr model.newUserModal
            , View.Compose.view model
                |> Element.inFront
                |> View.Common.whenAttr model.compose.modal
            , View.Modal.viewCookieConsent
                |> View.Common.when isDesktop
                |> Element.inFront
                |> View.Common.whenAttr (not model.cookieConsentGranted)
            ]
    ]
        |> column
            [ width fill
            , height fill
            , Background.image "./img/smoke-bg.jpg"
            ]


header : Model -> Element Msg
header model =
    let
        sidePadding =
            if model.dProfile == EH.Mobile then
                paddingXY 30 0

            else
                paddingXY 100 0
    in
    [ Input.button [ hover ]
        { onPress = Just <| GotoView ViewHome
        , label =
            Element.image
                [ height <| px 50
                ]
                { src = "./img/smokesignal-logo-horizontal.svg"
                , description = "smokesignal logo"
                }
        }
    , Input.text
        [ fill |> Element.maximum 350 |> width
        , Background.color black
        , View.Attrs.whiteGlowAttributeSmall
        ]
        { onChange = always ClickHappened
        , label = Input.labelHidden ""
        , placeholder =
            "Search . . ."
                |> text
                |> Input.placeholder []
                |> Just
        , text = ""
        }
        |> View.Common.when False
    , [ maybeTxTracker
            model.dProfile
            model.showExpandedTrackedTxs
            model.trackedTxs
            |> Maybe.withDefault Element.none
      , Element.newTabLink [ hover ]
            { url = "https://foundrydao.com/"
            , label =
                Element.image [ height <| px 50 ]
                    { src = "./img/foundry-icon.svg"
                    , description = ""
                    }
            }
      ]
        |> row [ spacing 40 ]
    ]
        |> row
            [ Font.color Theme.orange
            , width fill
            , Element.spaceEvenly
            , Element.centerY
            ]
        |> el
            [ width fill
            , sidePadding
            , height <| px 80
            , Background.color EH.black
            , whiteGlowAttribute
            , EH.moveToFront
            ]


viewBody : Model -> Element Msg
viewBody model =
    case model.view of
        ViewHome ->
            View.Home.view model
                |> viewFrame model

        ViewTopics ->
            View.Topics.view model
                |> viewFrame model

        ViewCompose _ ->
            View.Compose.view model

        ViewPost postId ->
            let
                key =
                    Misc.postIdToKey postId
            in
            model.rootPosts
                |> Dict.get key
                |> Maybe.Extra.unwrap
                    (model.replyPosts
                        |> Dict.get key
                        |> Maybe.map .core
                    )
                    (.core >> Just)
                |> Maybe.Extra.unwrap
                    (appStatusMessage
                        theme.appStatusTextColor
                        "Loading post..."
                    )
                    (View.PostPage.view model)
                |> viewFrame model

        ViewTopic topic ->
            View.Topic.view model topic
                |> viewFrame model


viewFrame : Model -> Element Msg -> Element Msg
viewFrame model elem =
    if model.dProfile == EH.Mobile then
        elem

    else
        [ banner
        , [ elem
          , View.Sidebar.view model
          ]
            |> row
                [ width fill
                , height fill
                , spacing 10
                ]
        ]
            |> column
                ([ width fill
                 , height fill
                 , spacing 10
                 ]
                    ++ (List.map Element.inFront <|
                            viewModals
                                --model.showNewToSmokeSignalModal
                                False
                       )
                )


banner : Element Msg
banner =
    Element.image
        [ height <| px 175
        , Background.color EH.black
        , whiteGlowAttribute
        , centerX
        ]
        { src = "./img/banner.png"
        , description = "Never be silenced"
        }


viewModals : Bool -> List (Element Msg)
viewModals showNewToSmokeSignalModal =
    Maybe.Extra.values
        [ if showNewToSmokeSignalModal == True then
            --Just <|
            --EH.modal
            --(Element.rgba 0 0 0 0.25)
            --False
            --CloseNewToSmokeSignalModal
            --CloseNewToSmokeSignalModal
            --<|
            --viewNewToSmokeSignalModal dProfile
            Nothing

          else
            Nothing
        ]


modals : Model -> List (Element Msg)
modals model =
    Maybe.Extra.values
        ([--  ,
          --  let
          --     showDraftInProgressButton =
          --         case model.route of
          --             ViewCompose ->
          --                 False
          --             _ ->
          --                 --(model.showHalfComposeUX == False)
          --                 --&& (not <| Post.contentIsEmpty model.composeUXModel.content)
          --                 -- TODO
          --                 False
          --    in
          --    if showDraftInProgressButton then
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
          --     Nothing
          --    else
          --     Nothing
          --, maybeViewDraftModal model
         ]
            ++ List.map Just
                (userNoticeEls
                    model.dProfile
                    model.userNotices
                )
        )


maybeTxTracker : DisplayProfile -> Bool -> Dict.Dict String TrackedTx -> Maybe (Element Msg)
maybeTxTracker dProfile showExpanded trackedTxs_ =
    if Dict.isEmpty trackedTxs_ then
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

            trackedTxs =
                trackedTxs_
                    |> Dict.values

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
        [ Background.color <| Element.rgb 0 0 0
        , Border.width 1
        , Border.color <| Element.rgb 1 1 1
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
        linkTextColor =
            Element.rgb 0.5 0.5 1

        etherscanLink label =
            Element.newTabLink
                [ Font.italic
                , Font.color linkTextColor
                ]
                { url = EthHelpers.etherscanTxUrl trackedTx.txHash
                , label = Element.text label
                }

        titleEl =
            case ( trackedTx.txInfo, trackedTx.status ) of
                ( TipTx postId _, _ ) ->
                    Element.row
                        []
                        [ Element.text "Tip "
                        , Element.el
                            [ Font.color linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                GotoView <|
                                    ViewPost postId
                            ]
                            (Element.text "Post")
                        ]

                ( BurnTx postId _, _ ) ->
                    Element.row
                        []
                        [ Element.text "Burn for "
                        , Element.el
                            [ Font.color linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                GotoView <|
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
                            [ Font.color linkTextColor
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
                        PostTx _ ->
                            case maybePostId of
                                Just postId ->
                                    Element.el
                                        [ Font.color theme.linkTextColor
                                        , Element.pointer
                                        , Element.Events.onClick <|
                                            GotoView <|
                                                ViewPost postId
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


maxContentColWidth : Int
maxContentColWidth =
    1200
