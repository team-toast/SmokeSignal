module State exposing (init, subscriptions, update)

import Array exposing (Array)
import Browser
import Browser.Events
import Browser.Navigation
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View
import ComposeUX.State as ComposeUX
import ComposeUX.Types as ComposeUX
import Config
import Contracts.Dai as Dai
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator exposing (mutateInfoGenerator)
import Dict exposing (Dict)
import Eth
import Eth.Decode
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry
import Eth.Types exposing (Address, TxHash)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..))
import Home.State as Home
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Ports exposing (connectToWeb3, consentToCookies, gTagOut, setDescription, txIn, txOut, walletSentryPort)
import Post exposing (Post)
import PostUX.State as PostUX
import Random
import Routing exposing (Route)
import Task
import Time
import TokenValue exposing (TokenValue)
import TopicUX.State as TopicUX
import TopicUX.Types as TopicUX
import Types exposing (..)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet


init :
    Flags
    -> Url
    -> Browser.Navigation.Key
    -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Routing.urlToRoute url

        ( wallet, walletNotices ) =
            if flags.networkId == 0 then
                ( Wallet.NoneDetected
                , [ UN.noWeb3Provider ]
                )

            else
                ( Wallet.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId
                , []
                )

        txSentry =
            TxSentry.init
                ( txOut, txIn )
                TxSentryMsg
                Config.httpProviderUrl

        ( initEventSentry, initEventSentryCmd ) =
            EventSentry.init EventSentryMsg Config.httpProviderUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            fetchPostsFromBlockrangeCmd
                (Eth.Types.BlockNum Config.startScanBlock)
                Eth.Types.LatestBlock
                initEventSentry

        now =
            Time.millisToPosix flags.nowInMillis
    in
    { navKey = key
    , basePath = flags.basePath
    , route = route
    , wallet = wallet
    , now = now
    , dProfile = EH.screenWidthToDisplayProfile flags.width
    , txSentry = txSentry
    , eventSentry = eventSentry
    , publishedPosts = Dict.empty
    , postUX = Nothing
    , replies = []
    , mode = BlankMode
    , showHalfComposeUX = False
    , composeUXModel = ComposeUX.init now (Post.TopLevel Post.defaultTopic)
    , blockTimes = Dict.empty
    , showAddressId = Nothing
    , userNotices = walletNotices
    , trackedTxs = []
    , showExpandedTrackedTxs = False
    , draftModal = Nothing
    , demoPhaceSrc = initDemoPhaceSrc
    , donateChecked = True
    , cookieConsentGranted = flags.cookieConsent
    , maybeSeoDescription = Nothing
    , searchInput = ""
    , topicUXModel = Nothing
    }
        |> gotoRoute route
        |> Tuple.mapSecond
            (\routeCmd ->
                Cmd.batch
                    [ initEventSentryCmd
                    , secondEventSentryCmd
                    , routeCmd
                    ]
            )


initDemoPhaceSrc : String
initDemoPhaceSrc =
    "2222222222222222222222222228083888c8f222"


update :
    Msg
    -> Model
    -> ( Model, Cmd Msg )
update msg prevModel =
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl prevModel.navKey (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( prevModel, cmd )

        UrlChanged url ->
            prevModel |> updateFromPageRoute (url |> Routing.urlToRoute)

        Tick newTime ->
            ( { prevModel | now = newTime }, Cmd.none )

        Resize width _ ->
            ( { prevModel
                | dProfile =
                    EH.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        EveryFewSeconds ->
            ( prevModel
            , Wallet.userInfo prevModel.wallet
                |> Maybe.map
                    (\userInfo ->
                        fetchDaiBalanceAndAllowanceCmd userInfo.address
                    )
                |> Maybe.withDefault Cmd.none
            )

        ShowExpandedTrackedTxs flag ->
            ( { prevModel
                | showExpandedTrackedTxs = flag
              }
            , Cmd.none
            )

        CheckTrackedTxsStatus ->
            ( prevModel
            , prevModel.trackedTxs
                |> List.filter
                    (\trackedTx ->
                        trackedTx.status == Mining
                    )
                |> List.map .txHash
                |> List.map (Eth.getTxReceipt Config.httpProviderUrl)
                |> List.map (Task.attempt TrackedTxStatusResult)
                |> Cmd.batch
            )

        TrackedTxStatusResult txReceiptResult ->
            case txReceiptResult of
                Err errStr ->
                    -- Hasn't yet been mined; make no change
                    ( prevModel, Cmd.none )

                Ok txReceipt ->
                    let
                        ( newStatus, maybePublishedPost, maybeUserNotice ) =
                            handleTxReceipt txReceipt
                    in
                    prevModel
                        |> updateTrackedTxStatusIfMining
                            txReceipt.hash
                            newStatus
                        |> addUserNotices
                            ([ maybeUserNotice ] |> Maybe.Extra.values)
                        |> (case maybePublishedPost of
                                Just post ->
                                    addPost txReceipt.blockNumber post

                                Nothing ->
                                    \m ->
                                        ( m, Cmd.none )
                           )

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        ( newWallet, cmd ) =
                            case walletSentry.account of
                                Just newAddress ->
                                    if (prevModel.wallet |> Wallet.userInfo |> Maybe.map .address) == Just newAddress then
                                        ( prevModel.wallet
                                        , Cmd.none
                                        )

                                    else
                                        ( Wallet.Active <|
                                            UserInfo
                                                walletSentry.networkId
                                                newAddress
                                                Nothing
                                                Checking
                                        , fetchDaiBalanceAndAllowanceCmd newAddress
                                        )

                                Nothing ->
                                    ( Wallet.OnlyNetwork walletSentry.networkId
                                    , Cmd.none
                                    )
                    in
                    ( { prevModel
                        | wallet = newWallet
                      }
                    , cmd
                    )

                Err errStr ->
                    ( prevModel |> addUserNotice (UN.walletError errStr)
                    , Cmd.none
                    )

        TxSentryMsg subMsg ->
            let
                ( newTxSentry, subCmd ) =
                    TxSentry.update subMsg prevModel.txSentry
            in
            ( { prevModel | txSentry = newTxSentry }, subCmd )

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        prevModel.eventSentry
            in
            ( { prevModel
                | eventSentry =
                    newEventSentry
              }
            , cmd
            )

        PostLogReceived log ->
            let
                decodedEventLog =
                    Eth.Decode.event SSContract.messageBurnDecoder log
            in
            case decodedEventLog.returnData of
                Err err ->
                    ( prevModel |> addUserNotice (UN.eventDecodeError err)
                    , Cmd.none
                    )

                Ok ssPost ->
                    let
                        ( interimModel, newPostCmd ) =
                            prevModel
                                |> addPost log.blockNumber
                                    (SSContract.fromMessageBurn
                                        log.transactionHash
                                        log.blockNumber
                                        Common.View.renderContentOrError
                                        ssPost
                                    )
                    in
                    ( interimModel
                        |> updateTrackedTxByTxHash
                            log.transactionHash
                            (\trackedTx ->
                                { trackedTx
                                    | status =
                                        Mined <|
                                            Just <|
                                                Post.Id
                                                    log.blockNumber
                                                    ssPost.hash
                                }
                            )
                    , Cmd.batch
                        [ newPostCmd
                        , getBlockTimeIfNeededCmd prevModel.blockTimes log.blockNumber
                        ]
                    )

        PostAccountingFetched postId fetchResult ->
            case fetchResult of
                Ok accounting ->
                    ( { prevModel
                        | publishedPosts =
                            prevModel.publishedPosts
                                |> updatePublishedPost postId
                                    (\publishedPost ->
                                        { publishedPost
                                            | maybeAccounting = Just accounting
                                            , core =
                                                publishedPost.core
                                                    |> (\c ->
                                                            { c
                                                                | author = accounting.firstAuthor
                                                            }
                                                       )
                                        }
                                    )
                      }
                    , Cmd.none
                    )

                Err httpErr ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "DAI balance" httpErr)
                    , Cmd.none
                    )

        BalanceFetched address fetchResult ->
            let
                maybeCurrentAddress =
                    Wallet.userInfo prevModel.wallet
                        |> Maybe.map .address
            in
            if maybeCurrentAddress /= Just address then
                ( prevModel, Cmd.none )

            else
                case fetchResult of
                    Ok balance ->
                        let
                            newWallet =
                                prevModel.wallet |> Wallet.withFetchedBalance balance
                        in
                        ( { prevModel
                            | wallet = newWallet
                          }
                        , Cmd.none
                        )

                    Err httpErr ->
                        ( prevModel
                            |> addUserNotice (UN.web3FetchError "DAI balance" httpErr)
                        , Cmd.none
                        )

        AllowanceFetched address fetchResult ->
            let
                maybeCurrentAddress =
                    Wallet.userInfo prevModel.wallet
                        |> Maybe.map .address
            in
            if maybeCurrentAddress /= Just address then
                ( prevModel, Cmd.none )

            else
                case fetchResult of
                    Ok allowance ->
                        let
                            isUnlocked =
                                if TokenValue.isMaxTokenValue allowance then
                                    True

                                else
                                    False

                            newWallet =
                                if isUnlocked then
                                    prevModel.wallet |> Wallet.withUnlockStatus Unlocked

                                else if Wallet.unlockStatus prevModel.wallet /= Unlocking then
                                    prevModel.wallet |> Wallet.withUnlockStatus Locked

                                else
                                    prevModel.wallet
                        in
                        ( { prevModel
                            | wallet =
                                newWallet
                          }
                        , Cmd.none
                        )

                    Err httpErr ->
                        ( prevModel
                            |> addUserNotice (UN.web3FetchError "DAI unlock status" httpErr)
                        , Cmd.none
                        )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err httpErr ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "block time" httpErr)
                    , Cmd.none
                    )

                Ok time ->
                    ( { prevModel
                        | blockTimes =
                            prevModel.blockTimes
                                |> Dict.insert blocknum time
                      }
                    , Cmd.none
                    )

        RestoreDraft draft ->
            { prevModel
                | draftModal = Nothing
                , composeUXModel =
                    prevModel.composeUXModel
                        |> (\composeUXModel ->
                                { composeUXModel
                                    | content = draft.core.content
                                    , daiInput =
                                        draft.core.authorBurn
                                            |> TokenValue.toFloatString Nothing
                                }
                           )
            }
                |> (gotoRoute <| Routing.Compose draft.core.metadata.context)

        DismissNotice id ->
            ( { prevModel
                | userNotices =
                    prevModel.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        PostUXMsg postUXId postUXMsg ->
            let
                postUXModelBeforeMsg =
                    case prevModel.postUX of
                        Just ( prevPostUXId, prevPostUXModel ) ->
                            if prevPostUXId == postUXId then
                                prevPostUXModel

                            else
                                PostUX.init

                        Nothing ->
                            PostUX.init

                updateResult =
                    postUXModelBeforeMsg
                        |> PostUX.update postUXMsg
            in
            ( { prevModel
                | postUX =
                    Just <|
                        ( postUXId
                        , updateResult.newModel
                        )
              }
            , Cmd.map (PostUXMsg postUXId) updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        ComposeUXMsg composeUXMsg ->
            let
                updateResult =
                    prevModel.composeUXModel
                        |> ComposeUX.update composeUXMsg
            in
            ( { prevModel
                | composeUXModel =
                    updateResult.newModel
              }
            , Cmd.map ComposeUXMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        TopicUXMsg topicUXMsg ->
            let
                topicUXModelBeforeMsg =
                    case prevModel.topicUXModel of
                        Just prevTopicUXModel ->
                            prevTopicUXModel

                        Nothing ->
                            TopicUX.init

                updateResult =
                    topicUXModelBeforeMsg
                        |> TopicUX.update topicUXMsg
            in
            ( { prevModel
                | topicUXModel =
                    Just <|
                        updateResult.newModel
              }
            , Cmd.map TopicUXMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        HomeMsg homeMsg ->
            case prevModel.mode of
                Home homeModel ->
                    let
                        updateResult =
                            homeModel
                                |> Home.update homeMsg
                    in
                    ( { prevModel
                        | mode =
                            Home updateResult.newModel
                      }
                    , Cmd.map HomeMsg updateResult.cmd
                    )
                        |> withMsgUps updateResult.msgUps

                _ ->
                    ( prevModel, Cmd.none )

        TxSigned txInfo txHashResult ->
            case txHashResult of
                Ok txHash ->
                    let
                        maybeNewRouteAndComposeModel =
                            case txInfo of
                                PostTx draft ->
                                    Just <|
                                        ( Routing.ViewContext <| postContextToViewContext prevModel.composeUXModel.context
                                        , prevModel.composeUXModel |> ComposeUX.resetModel
                                        )

                                _ ->
                                    Nothing

                        newPostUX =
                            case txInfo of
                                TipTx _ _ ->
                                    Nothing

                                BurnTx _ _ ->
                                    Nothing

                                _ ->
                                    prevModel.postUX

                        newWallet =
                            case txInfo of
                                UnlockTx ->
                                    prevModel.wallet
                                        |> Wallet.withUnlockStatus Unlocking

                                _ ->
                                    prevModel.wallet

                        interimModel =
                            { prevModel
                                | showExpandedTrackedTxs = True
                                , postUX = newPostUX
                                , wallet = newWallet
                            }
                                |> addTrackedTx txHash txInfo
                    in
                    case maybeNewRouteAndComposeModel of
                        Just ( route, composeUXModel ) ->
                            { interimModel
                                | composeUXModel = composeUXModel
                            }
                                |> gotoRoute route

                        Nothing ->
                            ( interimModel
                            , Cmd.none
                            )

                Err errStr ->
                    ( prevModel
                        |> addUserNotice
                            (UN.web3SigError
                                (txInfoToNameStr txInfo)
                                errStr
                            )
                    , Cmd.none
                    )

        MsgUp msgUp ->
            prevModel |> handleMsgUp msgUp

        ViewDraft maybeDraft ->
            ( { prevModel
                | draftModal = maybeDraft
              }
            , Cmd.none
            )

        ChangeDemoPhaceSrc ->
            ( prevModel
              --, Random.generate MutateDemoSrcWith mutateInfoGenerator
            , Random.generate NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
            )

        NewDemoSrc src ->
            ( { prevModel | demoPhaceSrc = src }
            , Cmd.none
            )

        ClickHappened ->
            ( { prevModel
                | showAddressId = Nothing
                , showExpandedTrackedTxs = False
                , draftModal = Nothing
              }
            , Cmd.none
            )

        CookieConsentGranted ->
            ( { prevModel
                | cookieConsentGranted = True
              }
            , Cmd.batch
                [ consentToCookies ()
                , gTagOut <|
                    encodeGTag <|
                        GTagData
                            "accept cookies"
                            ""
                            ""
                            0
                ]
            )


withAnotherUpdate : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withAnotherUpdate updateFunc ( firstModel, firstCmd ) =
    updateFunc firstModel
        |> (\( finalModel, secondCmd ) ->
                ( finalModel
                , Cmd.batch
                    [ firstCmd
                    , secondCmd
                    ]
                )
           )


handleMsgUp : MsgUp -> Model -> ( Model, Cmd Msg )
handleMsgUp msgUp prevModel =
    case msgUp of
        GotoRoute route ->
            prevModel
                |> gotoRoute route
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Browser.Navigation.pushUrl
                                prevModel.navKey
                                (Routing.routeToString prevModel.basePath route)
                            ]
                    )

        ConnectToWeb3 ->
            case prevModel.wallet of
                Wallet.NoneDetected ->
                    ( prevModel |> addUserNotice UN.cantConnectNoWeb3
                    , Cmd.none
                    )

                _ ->
                    ( prevModel
                    , connectToWeb3 ()
                    )

        ShowOrHideAddress phaceId ->
            ( { prevModel
                | showAddressId =
                    if prevModel.showAddressId == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
            )

        StartInlineCompose composeContext ->
            case prevModel.dProfile of
                Desktop ->
                    ( { prevModel
                        | showHalfComposeUX = True
                        , composeUXModel =
                            prevModel.composeUXModel
                                |> ComposeUX.updateContext composeContext
                      }
                    , Cmd.none
                    )

                Mobile ->
                    prevModel
                        |> (gotoRoute <|
                                Routing.Compose composeContext
                           )

        ExitCompose ->
            case prevModel.mode of
                Compose ->
                    prevModel
                        |> gotoRoute (Routing.ViewContext <| postContextToViewContext prevModel.composeUXModel.context)

                _ ->
                    ( { prevModel
                        | showHalfComposeUX = False
                      }
                    , Cmd.none
                    )

        AddUserNotice userNotice ->
            ( prevModel |> addUserNotice userNotice
            , Cmd.none
            )

        UnlockDai ->
            let
                txParams =
                    Dai.unlockDaiCall
                        |> Eth.toSend

                listeners =
                    { onMined = Nothing
                    , onSign = Just <| TxSigned UnlockTx
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        SubmitPost postDraft ->
            let
                txParams =
                    postDraft
                        |> Post.encodeDraft
                        |> SSContract.burnEncodedPost
                        |> Eth.toSend

                listeners =
                    { onMined = Nothing
                    , onSign = Just <| TxSigned <| PostTx postDraft
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        SubmitBurn postId amount ->
            let
                txParams =
                    SSContract.burnForPost postId.messageHash amount prevModel.donateChecked
                        |> Eth.toSend

                listeners =
                    { onMined = Nothing
                    , onSign = Just <| TxSigned <| BurnTx postId amount
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        SubmitTip postId amount ->
            let
                txParams =
                    SSContract.tipForPost postId.messageHash amount prevModel.donateChecked
                        |> Eth.toSend

                listeners =
                    { onMined = Nothing
                    , onSign = Just <| TxSigned <| TipTx postId amount
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        DonationCheckboxSet flag ->
            ( { prevModel
                | donateChecked = flag
              }
            , Cmd.none
            )

        NoOp ->
            ( prevModel, Cmd.none )


handleTxReceipt :
    Eth.Types.TxReceipt
    -> ( TxStatus, Maybe Post.Published, Maybe UserNotice )
handleTxReceipt txReceipt =
    case txReceipt.status of
        Just True ->
            let
                maybePostEvent =
                    txReceipt.logs
                        |> List.map (Eth.Decode.event SSContract.messageBurnDecoder)
                        |> List.map .returnData
                        |> List.map Result.toMaybe
                        |> Maybe.Extra.values
                        |> List.head
            in
            ( Mined <|
                Maybe.map
                    (\ssEvent ->
                        Post.Id
                            txReceipt.blockNumber
                            ssEvent.hash
                    )
                    maybePostEvent
            , Maybe.map
                (SSContract.fromMessageBurn
                    txReceipt.hash
                    txReceipt.blockNumber
                    Common.View.renderContentOrError
                )
                maybePostEvent
            , Nothing
            )

        Just False ->
            ( Failed MinedButExecutionFailed
            , Nothing
            , Nothing
            )

        Nothing ->
            ( Mining
            , Nothing
            , Just <|
                UN.unexpectedError "Weird. I Got a transaction receipt with a success value of 'Nothing'. Depending on why this happened I might be a little confused about any mining transactions." txReceipt
            )


addTrackedTx :
    TxHash
    -> TxInfo
    -> Model
    -> Model
addTrackedTx txHash txInfo prevModel =
    { prevModel
        | trackedTxs =
            prevModel.trackedTxs
                |> List.append
                    [ TrackedTx
                        txHash
                        txInfo
                        Mining
                    ]
    }


updateTrackedTxStatusIfMining :
    TxHash
    -> TxStatus
    -> Model
    -> Model
updateTrackedTxStatusIfMining txHash newStatus =
    updateTrackedTxIf
        (\trackedTx ->
            (trackedTx.txHash == txHash)
                && (trackedTx.status == Mining)
        )
        (\trackedTx ->
            { trackedTx
                | status = newStatus
            }
        )


withMsgUp :
    MsgUp
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
withMsgUp msgUp ( prevModel, prevCmd ) =
    handleMsgUp msgUp prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


handleMsgUps :
    List MsgUp
    -> Model
    -> ( Model, Cmd Msg )
handleMsgUps msgUps prevModel =
    List.foldl
        withMsgUp
        ( prevModel, Cmd.none )
        msgUps


withMsgUps :
    List MsgUp
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
withMsgUps msgUps ( prevModel, prevCmd ) =
    handleMsgUps msgUps prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


updateFromPageRoute :
    Route
    -> Model
    -> ( Model, Cmd Msg )
updateFromPageRoute route model =
    if model.route == route then
        ( model
        , Cmd.none
        )

    else
        gotoRoute route model


gotoRoute :
    Route
    -> Model
    -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    (case route of
        Routing.Home ->
            let
                ( homeModel, homeCmd ) =
                    Home.init
            in
            ( { prevModel
                | route = route
                , mode = Home homeModel
                , showHalfComposeUX = False
              }
            , Cmd.map HomeMsg homeCmd
            )

        Routing.Compose context ->
            ( { prevModel
                | route = route
                , mode = Compose
                , showHalfComposeUX = False
                , composeUXModel =
                    prevModel.composeUXModel |> ComposeUX.updateContext context
              }
            , Cmd.none
            )

        Routing.ViewContext context ->
            ( { prevModel
                | route = route
                , mode = ViewContext context
              }
            , Maybe.map
                setDescription
                (viewContextToMaybeDescription prevModel.publishedPosts context)
                |> Maybe.withDefault Cmd.none
            )

        Routing.NotFound err ->
            ( { prevModel
                | route = route
              }
                |> addUserNotice UN.routeNotFound
            , Cmd.none
            )
    )
        |> withAnotherUpdate updateSeoDescriptionIfNeededCmd


addPost :
    Int
    -> Post.Published
    -> Model
    -> ( Model, Cmd Msg )
addPost blockNumber publishedPost prevModel =
    let
        alreadyHavePost =
            prevModel.publishedPosts
                |> Dict.get blockNumber
                |> Maybe.map
                    (List.any
                        (\listedPost ->
                            listedPost.id == publishedPost.id
                        )
                    )
                |> Maybe.withDefault False
    in
    if alreadyHavePost then
        ( prevModel, Cmd.none )

    else
        ( { prevModel
            | publishedPosts =
                prevModel.publishedPosts
                    |> Dict.update blockNumber
                        (\maybePostsForBlock ->
                            Just <|
                                case maybePostsForBlock of
                                    Nothing ->
                                        [ publishedPost ]

                                    Just posts ->
                                        List.append posts [ publishedPost ]
                        )
            , replies =
                List.append
                    prevModel.replies
                    (case Post.contextReplyTo publishedPost.core.metadata.context of
                        Just replyTo ->
                            [ { from = publishedPost.id
                              , to = replyTo
                              }
                            ]

                        Nothing ->
                            []
                    )
          }
        , SSContract.getAccountingCmd
            publishedPost.id.messageHash
            (PostAccountingFetched publishedPost.id)
        )
            |> withAnotherUpdate updateSeoDescriptionIfNeededCmd


getBlockTimeIfNeededCmd :
    Dict Int Time.Posix
    -> Int
    -> Cmd Msg
getBlockTimeIfNeededCmd blockTimes blockNumber =
    if Dict.get blockNumber blockTimes == Nothing then
        getBlockTimeCmd blockNumber

    else
        Cmd.none


updateSeoDescriptionIfNeededCmd :
    Model
    -> ( Model, Cmd Msg )
updateSeoDescriptionIfNeededCmd model =
    let
        appropriateMaybeDescription =
            case model.mode of
                BlankMode ->
                    Nothing

                Home _ ->
                    Nothing

                Compose ->
                    Nothing

                ViewContext context ->
                    viewContextToMaybeDescription model.publishedPosts context
    in
    if appropriateMaybeDescription /= model.maybeSeoDescription then
        ( { model
            | maybeSeoDescription = appropriateMaybeDescription
          }
        , setDescription (appropriateMaybeDescription |> Maybe.withDefault defaultSeoDescription)
        )

    else
        ( model, Cmd.none )


fetchPostsFromBlockrangeCmd :
    Eth.Types.BlockId
    -> Eth.Types.BlockId
    -> EventSentry Msg
    -> ( EventSentry Msg, Cmd Msg, EventSentry.Ref )
fetchPostsFromBlockrangeCmd from to sentry =
    EventSentry.watch
        PostLogReceived
        sentry
    <|
        SSContract.messageBurnEventFilter
            from
            to
            Nothing
            Nothing


fetchDaiBalanceAndAllowanceCmd :
    Address
    -> Cmd Msg
fetchDaiBalanceAndAllowanceCmd address =
    Cmd.batch
        [ Dai.getAllowanceCmd address (AllowanceFetched address)
        , Dai.getBalanceCmd address (BalanceFetched address)
        ]


getBlockTimeCmd :
    Int
    -> Cmd Msg
getBlockTimeCmd blocknum =
    Eth.getBlock
        Config.httpProviderUrl
        blocknum
        |> Task.map .timestamp
        |> Task.attempt (BlockTimeFetched blocknum)


addUserNotice :
    UserNotice
    -> Model
    -> Model
addUserNotice notice model =
    model
        |> addUserNotices [ notice ]


addUserNotices :
    List UserNotice
    -> Model
    -> Model
addUserNotices notices model =
    { model
        | userNotices =
            List.append
                model.userNotices
                notices
                |> List.Extra.uniqueBy .uniqueLabel
    }


encodeGTag :
    GTagData
    -> Json.Decode.Value
encodeGTag gtag =
    Json.Encode.object
        [ ( "event", Json.Encode.string gtag.event )
        , ( "category", Json.Encode.string gtag.category )
        , ( "label", Json.Encode.string gtag.label )
        , ( "value", Json.Encode.int gtag.value )
        ]


subscriptions :
    Model
    -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , Time.every 2000 (always ChangeDemoPhaceSrc)
        , Time.every 2500 (always EveryFewSeconds)
        , Time.every 5000 (always CheckTrackedTxsStatus)
        , walletSentryPort
            (WalletSentry.decodeToMsg
                (WalletStatus << Err)
                (WalletStatus << Ok)
            )
        , TxSentry.listen model.txSentry
        , Browser.Events.onResize Resize
        , Sub.map ComposeUXMsg ComposeUX.subscriptions
        ]
