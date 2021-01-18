module Update exposing (update)

--import PostUX.State as PostUX
--import TopicUX.State as TopicUX
--import TopicUX.Types as TopicUX
--import Home.State as Home
--import ComposeUX.State as ComposeUX
--import ComposeUX.Types as ComposeUX

import Browser
import Browser.Navigation
import Config
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, TxHash)
import Helpers.Element as EH exposing (DisplayProfile(..))
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Misc exposing (defaultSeoDescription, txInfoToNameStr, updatePublishedPost)
import Ports exposing (connectToWeb3, consentToCookies, gTagOut, setDescription)
import Post
import Random
import Routing
import Task
import Time
import TokenValue
import Types exposing (GTagData, Id, Model, Msg(..), Published, Route(..), TrackedTx, TxInfo(..), TxStatus(..), UserInfo, View(..))
import Url
import UserNotice as UN exposing (UserNotice)
import View.Common
import Wallet


update : Msg -> Model -> ( Model, Cmd Msg )
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
            let
                route =
                    url
                        |> Routing.urlToRoute
            in
            case route of
                Home ->
                    ( { prevModel
                        | view = ViewHome
                      }
                    , Cmd.none
                    )

                Compose context ->
                    ( { prevModel
                        | view = ViewCompose
                      }
                    , Cmd.none
                    )

                RouteViewContext ->
                    ( { prevModel
                        | view = prevModel.view
                      }
                    , Cmd.none
                    )

                RouteTopic str ->
                    ( { prevModel
                        | view = ViewTopic str
                      }
                    , Cmd.none
                    )

                NotFound err ->
                    ( { prevModel
                        | userNotices = [ UN.routeNotFound ]
                      }
                    , Cmd.none
                    )

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
                                        --( Types.Active <|
                                        --UserInfo
                                        --walletSentry.networkId
                                        --newAddress
                                        --Nothing
                                        --Checking
                                        ( prevModel.wallet
                                        , fetchDaiBalanceAndAllowanceCmd newAddress
                                        )

                                Nothing ->
                                    ( Types.OnlyNetwork walletSentry.networkId
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
                                        View.Common.renderContentOrError
                                        ssPost
                                    )
                    in
                    ( interimModel
                      --|> updateTrackedTxByTxHash
                      --log.transactionHash
                      --(\trackedTx ->
                      --{ trackedTx
                      --| status =
                      --Mined <|
                      --Just <|
                      --Id
                      --log.blockNumber
                      --ssPost.hash
                      --}
                      --)
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
                                    --prevModel.wallet |> Wallet.withUnlockStatus Unlocked
                                    prevModel.wallet

                                else if False then
                                    --else if Wallet.unlockStatus prevModel.wallet /= Unlocking then
                                    --prevModel.wallet |> Wallet.withUnlockStatus Locked
                                    prevModel.wallet

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

                --, composeUXModel =
                --prevModel.composeUXModel
                --|> (\composeUXModel ->
                --{ composeUXModel
                --| content = draft.core.content
                --, daiInput =
                --draft.core.authorBurn
                --|> TokenValue.toFloatString Nothing
                --}
                --)
                -- TODO
                --|> identity
            }
                |> (gotoRoute <| Compose draft.core.metadata.context)

        DismissNotice id ->
            ( { prevModel
                | userNotices =
                    prevModel.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        --PostUXMsg postUXId postUXMsg ->
        --let
        --postUXModelBeforeMsg =
        ---- TODO
        --case Nothing of
        --Just ( prevPostUXId, prevPostUXModel ) ->
        --if prevPostUXId == postUXId then
        --prevPostUXModel
        --else
        --PostUX.init
        --Nothing ->
        --PostUX.init
        --updateResult =
        --postUXModelBeforeMsg
        --|> PostUX.update postUXMsg
        --in
        --( prevModel
        -- TODO
        --| postUX =
        --Just <|
        --( postUXId
        --, updateResult.newModel
        --)
        --, Cmd.map (PostUXMsg postUXId) updateResult.cmd
        --)
        --|> withMsgUps updateResult.msgUps
        --ComposeUXMsg composeUXMsg ->
        --let
        --updateResult =
        --prevModel.composeUXModel
        --Nothing
        --TODO
        --|> ComposeUX.update composeUXMsg
        --in
        --( prevModel
        --| composeUXModel =
        --updateResult.newModel
        -- TODO
        --prevModel.composeUXModel
        --}
        --, Cmd.map ComposeUXMsg updateResult.cmd
        --, Cmd.none
        --)
        --|> withMsgUps updateResult.msgUps
        --TopicUXMsg topicUXMsg ->
        --let
        --topicUXModelBeforeMsg =
        --case prevModel.topicUXModel of
        --Just prevTopicUXModel ->
        --prevTopicUXModel
        --Nothing ->
        --TopicUX.init
        --updateResult =
        --topicUXModelBeforeMsg
        --|> TopicUX.update topicUXMsg
        --in
        --( { prevModel
        --| topicUXModel =
        --Just <|
        --updateResult.newModel
        --}
        --, Cmd.map TopicUXMsg updateResult.cmd
        --)
        --|> withMsgUps updateResult.msgUps
        --( prevModel, Cmd.none )
        --HomeMsg homeMsg ->
        --case prevModel.mode of
        --ModeHome homeModel ->
        --let
        --updateResult =
        --homeModel
        --|> Home.update homeMsg
        --in
        --( { prevModel
        --| view =
        --ModeHome updateResult.newModel
        --}
        --, Cmd.map HomeMsg updateResult.cmd
        --)
        --|> withMsgUps updateResult.msgUps
        --_ ->
        --( prevModel, Cmd.none )
        TxSigned txInfo txHashResult ->
            case txHashResult of
                Ok txHash ->
                    let
                        maybeNewRouteAndComposeModel =
                            case txInfo of
                                PostTx draft ->
                                    -- TODO
                                    --Just <|
                                    --( Routing.ViewContext <| postContextToViewContext prevModel.composeUXModel.context
                                    --, prevModel.composeUXModel |> ComposeUX.resetModel
                                    --)
                                    Nothing

                                _ ->
                                    Nothing

                        newPostUX =
                            case txInfo of
                                TipTx _ _ ->
                                    Nothing

                                BurnTx _ _ ->
                                    Nothing

                                _ ->
                                    --prevModel.postUX
                                    Nothing

                        interimModel =
                            { prevModel
                                | showExpandedTrackedTxs = True

                                --, postUX = newPostUX
                                --, wallet = newWallet
                            }
                                |> addTrackedTx txHash txInfo
                    in
                    case maybeNewRouteAndComposeModel of
                        Just ( route, composeUXModel ) ->
                            --{ interimModel
                            --| composeUXModel = composeUXModel
                            --}
                            --|> gotoRoute route
                            ( interimModel, Cmd.none )

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

        GotoRoute route ->
            --prevModel
            --|> gotoRoute route
            --|> Tuple.mapSecond
            --(\cmd ->
            --Cmd.batch
            --[ cmd
            --, Browser.Navigation.pushUrl
            --prevModel.navKey
            --(Routing.routeToString prevModel.basePath route)
            --]
            --)
            ( prevModel
            , Browser.Navigation.pushUrl
                prevModel.navKey
                (Routing.routeToString prevModel.basePath route)
            )

        ConnectToWeb3 ->
            case prevModel.wallet of
                Types.NoneDetected ->
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

                        --, composeUXModel =
                        --prevModel.composeUXModel
                        -- TODO
                        --|> ComposeUX.updateContext composeContext
                      }
                    , Cmd.none
                    )

                Mobile ->
                    prevModel
                        |> (gotoRoute <|
                                Compose composeContext
                           )

        ExitCompose ->
            case prevModel.view of
                ViewCompose ->
                    -- TODO
                    ( prevModel, Cmd.none )

                --|> gotoRoute (Routing.ViewContext <| postContextToViewContext prevModel.composeUXModel.context)
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
            --let
            --txParams =
            --Dai.unlockDaiCall
            --|> Eth.toSend
            --listeners =
            --{ onMined = Nothing
            --, onSign = Just <| TxSigned UnlockTx
            --, onBroadcast = Nothing
            --}
            --( txSentry, cmd ) =
            --TxSentry.customSend prevModel.txSentry listeners txParams
            --in
            ( { prevModel
                | txSentry = prevModel.txSentry
              }
            , Cmd.none
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


gotoRoute : Route -> Model -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    (case route of
        Home ->
            --let
            --( homeModel, homeCmd ) =
            --Home.init
            --in
            --( { prevModel
            --| route = route
            --, view = ModeHome homeModel
            --, showHalfComposeUX = False
            --}
            --, Cmd.map HomeMsg homeCmd
            --)
            ( prevModel, Cmd.none )

        Compose context ->
            ( { prevModel
                | route = route

                --, view = ModeCompose
                , showHalfComposeUX = False

                --, composeUXModel =
                --prevModel.composeUXModel
                --|> ComposeUX.updateContext context
                -- TODO
              }
            , Cmd.none
            )

        RouteViewContext ->
            ( { prevModel
                | route = route

                --, view = ViewContext context
              }
              --, Maybe.map
              --setDescription
              --(viewContextToMaybeDescription prevModel.publishedPosts context)
              --|> Maybe.withDefault Cmd.none
            , Cmd.none
            )

        RouteTopic _ ->
            ( prevModel, Cmd.none )

        NotFound err ->
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
    -> Published
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


handleTxReceipt :
    Eth.Types.TxReceipt
    -> ( TxStatus, Maybe Published, Maybe UserNotice )
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
                        Id
                            txReceipt.blockNumber
                            ssEvent.hash
                    )
                    maybePostEvent
            , Maybe.map
                (SSContract.fromMessageBurn
                    txReceipt.hash
                    txReceipt.blockNumber
                    View.Common.renderContentOrError
                )
                maybePostEvent
            , Nothing
            )

        Just False ->
            ( Failed Types.MinedButExecutionFailed
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
    --updateTrackedTxIf
    --(\trackedTx ->
    --(trackedTx.txHash == txHash)
    --&& (trackedTx.status == Mining)
    --)
    --(\trackedTx ->
    --{ trackedTx
    --| status = newStatus
    --}
    --)
    identity


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
            case model.view of
                ViewHome ->
                    Nothing

                ViewCompose ->
                    Nothing

                --ViewContext context ->
                _ ->
                    --viewContextToMaybeDescription model.publishedPosts context
                    Nothing
    in
    if appropriateMaybeDescription /= model.maybeSeoDescription then
        ( { model
            | maybeSeoDescription = appropriateMaybeDescription
          }
        , setDescription (appropriateMaybeDescription |> Maybe.withDefault defaultSeoDescription)
        )

    else
        ( model, Cmd.none )


fetchDaiBalanceAndAllowanceCmd :
    Address
    -> Cmd Msg
fetchDaiBalanceAndAllowanceCmd address =
    Cmd.batch
        --[ Dai.getAllowanceCmd address (AllowanceFetched address)
        --, Dai.getBalanceCmd address (BalanceFetched address)
        --]
        []


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
