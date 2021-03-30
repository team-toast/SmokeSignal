module Update exposing (update)

import Array
import Browser
import Browser.Navigation
import Chain
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Types exposing (TxReceipt)
import Eth.Utils
import GTag exposing (GTagData, gTagOut, gTagOutOnlyOnLabelOrValueChange, gTagOutOnlyOnceForEvent)
import Helpers.Element as EH exposing (DisplayProfile(..))
import Http
import Json.Decode
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc exposing (emptyComposeModel, postIdToKey, sortTypeToString)
import Ports
import Post
import Process
import Random
import Result.Extra exposing (unpack)
import Routing exposing (viewUrlToPathString)
import Sentry
import Set
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet exposing (userInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ensureUserInfo fn =
            model.wallet
                |> Wallet.userInfo
                |> unwrap ( model, Ports.log "Missing wallet" ) fn
    in
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            pushUrlPathAndUpdateGtagAnalyticsCmd
                                model.navKey
                                ("#" ++ (url.fragment |> Maybe.withDefault "!"))

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( model, cmd )

        RouteChanged route ->
            handleRoute model route

        Tick newTime ->
            ( { model | now = newTime }, Cmd.none )

        PreviewSet val ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | preview = val })
              }
            , Cmd.none
            )

        Resize width _ ->
            ( { model
                | dProfile =
                    EH.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        ShowExpandedTrackedTxs flag ->
            let
                ( newGtagHistory, gtagCmd ) =
                    GTagData
                        "show expanded txs"
                        Nothing
                        ((if flag == True then
                            "True"

                          else
                            "False"
                         )
                            |> Just
                        )
                        Nothing
                        |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
            in
            ( { model
                | showExpandedTrackedTxs = flag
                , gtagHistory = newGtagHistory
              }
            , gtagCmd
            )

        PostResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\err ->
                                case err of
                                    Types.UserRejected ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "post tx rejected"
                                                    ("tx rejected" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | compose =
                                                model.compose
                                                    |> (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "Your transaction was cancelled."
                                                            }
                                                       )
                                          }
                                        , gtagCmd
                                        )

                                    Types.OtherErr e ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "post tx error"
                                                    ("tx error" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | compose =
                                                model.compose
                                                    |> (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "There has been a problem."
                                                            }
                                                       )
                                          }
                                        , [ Ports.log e
                                          , gtagCmd
                                          ]
                                            |> Cmd.batch
                                        )
                            )
                            (\txHash ->
                                let
                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "post tx confirmed"
                                            ("tx confirmed" |> Just)
                                            (Eth.Utils.txHashToString txHash
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | inProgress = False
                                                        , modal = False
                                                    }
                                               )
                                    , showExpandedTrackedTxs = True
                                    , userNotices =
                                        model.userNotices
                                            |> List.append [ UN.notify "Your transaction is mining." ]
                                    , trackedTxs =
                                        model.trackedTxs
                                            |> Dict.insert (Eth.Utils.txHashToString txHash)
                                                { txHash = txHash
                                                , txInfo = PostTx txHash
                                                , status = Mining
                                                , chain = userInfo.chain
                                                }
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )
                            )
                )

        ChainSwitchResponse res ->
            res
                |> unpack
                    (\err ->
                        case err of
                            Types.UserRejected ->
                                ( { model
                                    | chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            Types.OtherErr e ->
                                ( { model
                                    | chainSwitchInProgress = False
                                  }
                                , Ports.log e
                                )
                    )
                    (\() ->
                        ( -- Wait for WalletResponse to update model.chainSwitchInProgress
                          model
                        , Cmd.none
                        )
                    )

        BurnOrTipResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\err ->
                                case err of
                                    Types.UserRejected ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "burn/tip tx rejected"
                                                    ("tx rejected" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | postState =
                                                model.postState
                                                    |> Maybe.map
                                                        (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "Your transaction was cancelled."
                                                            }
                                                        )
                                          }
                                        , gtagCmd
                                        )

                                    Types.OtherErr e ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "burn/tip tx error"
                                                    ("tx error" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | postState =
                                                model.postState
                                                    |> Maybe.map
                                                        (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "There has been a problem."
                                                            }
                                                        )
                                          }
                                        , [ Ports.log e
                                          , gtagCmd
                                          ]
                                            |> Cmd.batch
                                        )
                            )
                            (\txHash ->
                                let
                                    trackedTx =
                                        model.postState
                                            |> Maybe.map
                                                (\state ->
                                                    { txHash = txHash
                                                    , txInfo =
                                                        case state.showInput of
                                                            Tip ->
                                                                TipTx state.id

                                                            Burn ->
                                                                BurnTx state.id
                                                    , status = Mining
                                                    , chain = userInfo.chain
                                                    }
                                                )

                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "burn/tip tx confirmed"
                                            ("tx confirmed" |> Just)
                                            (Eth.Utils.txHashToString txHash
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | postState = Nothing
                                    , userNotices =
                                        model.userNotices
                                            |> List.append [ UN.notify "Your transaction is mining." ]
                                    , trackedTxs =
                                        model.trackedTxs
                                            |> (trackedTx
                                                    |> unwrap identity
                                                        (Dict.insert
                                                            (Eth.Utils.txHashToString txHash)
                                                        )
                                               )
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )
                            )
                )

        CheckTrackedTxsStatus ->
            ( model
            , model.trackedTxs
                |> Dict.values
                |> List.filter
                    (\trackedTx ->
                        trackedTx.status == Mining
                    )
                |> List.map
                    (\tx ->
                        Misc.getTxReceipt
                            (Chain.getProviderUrl tx.chain model.config)
                            tx.txHash
                            |> Task.attempt TrackedTxStatusResult
                    )
                |> Cmd.batch
            )

        TrackedTxStatusResult res ->
            case res of
                Err err ->
                    ( model
                    , logHttpError "TrackedTxStatusResult" err
                    )

                Ok data ->
                    data
                        |> unwrap
                            ( model, Cmd.none )
                            (\txReceipt ->
                                model.trackedTxs
                                    |> Dict.get (Eth.Utils.txHashToString txReceipt.hash)
                                    |> unwrap ( model, Ports.log "Transaction not found." )
                                        (\tx ->
                                            let
                                                ( newStatus, _, maybeUserNotice ) =
                                                    handleTxReceipt tx.chain txReceipt

                                                isMined =
                                                    case newStatus of
                                                        Mined _ ->
                                                            True

                                                        _ ->
                                                            False

                                                fetchAccounting =
                                                    (case tx.txInfo of
                                                        -- Rely on PostLogReceived as source of truth for post data.
                                                        PostTx _ ->
                                                            --maybePublishedPost
                                                            --|> Maybe.map
                                                            --(\r ->
                                                            --case r of
                                                            --LogReply p ->
                                                            --p.core
                                                            --LogRoot p ->
                                                            --p.core
                                                            --)
                                                            Nothing

                                                        TipTx id ->
                                                            Misc.getPostOrReply id model
                                                                |> Maybe.map Misc.getCore

                                                        BurnTx id ->
                                                            Misc.getPostOrReply id model
                                                                |> Maybe.map Misc.getCore
                                                    )
                                                        |> unwrap Cmd.none
                                                            (fetchPostInfo model.blockTimes model.config)

                                                ( newGtagHistory, maybeGtagCmd ) =
                                                    if isMined then
                                                        let
                                                            ( actionName, label ) =
                                                                -- question:
                                                                -- is there a way to get postId here for a post?
                                                                case tx.txInfo of
                                                                    PostTx txHash ->
                                                                        ( "post tx mined"
                                                                        , Eth.Utils.txHashToString txHash
                                                                        )

                                                                    TipTx postId ->
                                                                        ( "tip tx mined"
                                                                        , Post.postIdToString postId
                                                                        )

                                                                    BurnTx postId ->
                                                                        ( "burn tx mined"
                                                                        , Post.postIdToString postId
                                                                        )
                                                        in
                                                        GTag.gTagOutOnlyOnLabelOrValueChange model.gtagHistory <|
                                                            GTagData
                                                                actionName
                                                                Nothing
                                                                (Just label)
                                                                Nothing

                                                    else
                                                        ( model.gtagHistory
                                                        , Cmd.none
                                                        )
                                            in
                                            ( { model
                                                | trackedTxs =
                                                    model.trackedTxs
                                                        |> Dict.update
                                                            (Eth.Utils.txHashToString txReceipt.hash)
                                                            (Maybe.map
                                                                (\info ->
                                                                    if info.status == Mining then
                                                                        { info | status = newStatus }

                                                                    else
                                                                        info
                                                                )
                                                            )
                                                , userNotices =
                                                    model.userNotices
                                                        ++ (maybeUserNotice
                                                                |> unwrap [] List.singleton
                                                           )
                                                , gtagHistory = newGtagHistory
                                              }
                                            , Cmd.batch
                                                [ if isMined then
                                                    fetchAccounting

                                                  else
                                                    Cmd.none
                                                , maybeGtagCmd
                                                ]
                                            )
                                        )
                            )

        WalletResponse res ->
            res
                |> unpack
                    (\err ->
                        case err of
                            WalletInProgress ->
                                ( { model
                                    | userNotices = UN.unexpectedError "Please complete the wallet connection process." :: model.userNotices
                                  }
                                , Cmd.none
                                )

                            WalletCancel ->
                                ( { model
                                    | userNotices = UN.unexpectedError "The wallet connection has been cancelled." :: model.userNotices
                                    , wallet = NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            NetworkNotSupported ->
                                ( { model
                                    | userNotices = UN.unexpectedError "This network is not supported by SmokeSignal." :: model.userNotices
                                    , wallet = NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            WalletError e ->
                                ( { model
                                    | wallet =
                                        Types.NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Ports.log e
                                )
                    )
                    (\info ->
                        let
                            ( gtagHistory, walletConnectedGtagCmd ) =
                                gTagOutOnlyOnceForEvent model.gtagHistory <|
                                    GTagData
                                        "wallet connected"
                                        Nothing
                                        (Just <| Eth.Utils.addressToString info.address)
                                        Nothing
                        in
                        ( { model
                            | wallet = Active info
                            , chainSwitchInProgress = False
                            , gtagHistory = gtagHistory
                            , compose =
                                model.compose
                                    |> (\r ->
                                            { r
                                                | message = Nothing
                                                , error = Nothing
                                            }
                                       )
                          }
                        , walletConnectedGtagCmd
                        )
                    )

        BalanceResponse val ->
            ensureUserInfo
                (\userInfo ->
                    let
                        fetchBalance =
                            Process.sleep 1000
                                |> Task.perform
                                    (\_ ->
                                        userInfo.address
                                            |> Eth.Utils.addressToString
                                            |> Ports.refreshWallet
                                            |> ExecuteDelayedCmd
                                    )
                    in
                    val
                        |> unwrap
                            ( { model
                                | wallet =
                                    Active { userInfo | xDaiStatus = XDaiStandby }
                              }
                            , Ports.log "Missing balance"
                            )
                            (\balance ->
                                let
                                    balanceEmpty =
                                        TokenValue.isZero balance

                                    shouldFetch =
                                        balanceEmpty && userInfo.xDaiStatus == WaitingForBalance

                                    compose =
                                        if shouldFetch then
                                            model.compose

                                        else
                                            model.compose
                                                |> (\r ->
                                                        { r
                                                            | message = Nothing
                                                            , error = Nothing
                                                        }
                                                   )

                                    wallet =
                                        Active
                                            { userInfo
                                                | balance = balance
                                                , xDaiStatus =
                                                    if shouldFetch then
                                                        WaitingForBalance

                                                    else
                                                        userInfo.xDaiStatus
                                            }
                                in
                                ( { model
                                    | wallet = wallet
                                    , compose = compose
                                  }
                                , if shouldFetch then
                                    fetchBalance

                                  else
                                    Cmd.none
                                )
                            )
                )

        ExecuteDelayedCmd cmd ->
            ( model, cmd )

        EventSentryMsg chain eventMsg ->
            case chain of
                Eth ->
                    let
                        ( newEventSentry, cmd ) =
                            model.sentries.ethereum
                                |> unwrap ( Nothing, Cmd.none )
                                    (Sentry.update
                                        eventMsg
                                    )
                    in
                    ( { model
                        | sentries =
                            model.sentries
                                |> (\ss ->
                                        { ss
                                            | ethereum =
                                                newEventSentry
                                        }
                                   )
                      }
                    , cmd
                    )

                XDai ->
                    let
                        ( newEventSentry, cmd ) =
                            model.sentries.xDai
                                |> unwrap ( Nothing, Cmd.none )
                                    (Sentry.update
                                        eventMsg
                                    )
                    in
                    ( { model
                        | sentries =
                            model.sentries
                                |> (\ss ->
                                        { ss
                                            | xDai =
                                                newEventSentry
                                        }
                                   )
                      }
                    , cmd
                    )

        PostLogReceived res ->
            case res.returnData of
                Err err ->
                    ( model
                    , err
                        |> Json.Decode.errorToString
                        |> String.left 200
                        |> (++) "PostLogReceived:\n"
                        |> Ports.log
                    )

                Ok log ->
                    --|> updateTrackedTxByTxHash
                    --log.transactionHash
                    --(\trackedTx ->
                    --{ trackedTx
                    --| status =
                    --Mined <|
                    --Just <|
                    --Post.Id
                    --log.blockNumber
                    --ssPost.hash
                    --}
                    --)
                    let
                        core =
                            Misc.getCore log
                    in
                    ( addPost log model
                    , fetchPostInfo model.blockTimes model.config core
                    )

        PostAccountingFetched postId res ->
            case res of
                Ok accountingData ->
                    let
                        key =
                            Misc.postIdToKey postId

                        maybeTopic =
                            model.rootPosts
                                |> Dict.get key
                                |> Maybe.map .topic

                        accounting =
                            model.accounting
                                |> Dict.insert key accountingData

                        updateTopics =
                            maybeTopic
                                |> unwrap identity
                                    (\topic ->
                                        Dict.update
                                            topic
                                            (unwrap
                                                { total = accountingData.totalBurned
                                                , ids = Set.singleton key
                                                }
                                                (\data ->
                                                    let
                                                        ids =
                                                            data.ids
                                                                |> Set.insert key

                                                        total =
                                                            if Set.size data.ids == Set.size ids then
                                                                data.total

                                                            else
                                                                ids
                                                                    |> Set.toList
                                                                    |> List.filterMap
                                                                        (\id ->
                                                                            Dict.get id accounting
                                                                        )
                                                                    |> List.map .totalBurned
                                                                    |> List.foldl
                                                                        TokenValue.add
                                                                        TokenValue.zero
                                                    in
                                                    { total = total
                                                    , ids = ids
                                                    }
                                                )
                                                >> Just
                                            )
                                    )
                    in
                    ( { model
                        | accounting = accounting
                        , topics =
                            model.topics
                                |> updateTopics
                        , pages =
                            model.rootPosts
                                |> calculatePagination
                                    model.sortType
                                    model.blockTimes
                                    accounting
                                    model.now
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , logHttpError "PostAccountingFetched" err
                    )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err err ->
                    ( model
                    , logHttpError "BlockTimeFetched" err
                    )

                Ok time ->
                    ( { model
                        | blockTimes =
                            model.blockTimes
                                |> Dict.insert blocknum time
                      }
                    , Cmd.none
                    )

        DismissNotice id ->
            ( { model
                | userNotices =
                    model.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        GotoView view ->
            let
                urlString =
                    Routing.viewUrlToPathString view
            in
            ( model
            , pushUrlPathAndUpdateGtagAnalyticsCmd
                model.navKey
                urlString
            )

        ConnectToWeb3 ->
            let
                gtagCmd =
                    GTagData
                        "wallet connect initiated"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | wallet = Connecting
              }
            , [ Ports.connectToWeb3 ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        ShowOrHideAddress phaceId ->
            ( { model
                | showAddressId =
                    if model.showAddressId == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
            )

        PriceResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\_ ->
                                let
                                    compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | inProgress = False
                                                        , error = Just "There has been a problem."
                                                    }
                                               )
                                in
                                ( { model | compose = compose }, Cmd.none )
                            )
                            (\price ->
                                model.compose.dollar
                                    |> getPostBurnAmount price
                                    |> Result.andThen
                                        (\burnAmount ->
                                            let
                                                donateAmount =
                                                    if model.compose.donate then
                                                        TokenValue.div burnAmount 100

                                                    else
                                                        TokenValue.zero

                                                lowBalance =
                                                    --TokenValue.compare
                                                    --(TokenValue.add burnAmount donateAmount)
                                                    --userInfo.balance
                                                    --/= LT
                                                    False

                                                context =
                                                    case model.compose.context of
                                                        Types.TopLevel topic ->
                                                            model.topicInput
                                                                |> Misc.validateTopic
                                                                |> Maybe.withDefault topic
                                                                |> TopLevel

                                                        ctx ->
                                                            ctx

                                                metadata =
                                                    { metadataVersion =
                                                        Post.currentMetadataVersion
                                                    , context = context
                                                    , maybeDecodeError = Nothing
                                                    }

                                                content =
                                                    { title =
                                                        if String.isEmpty model.compose.title then
                                                            Nothing

                                                        else
                                                            Just model.compose.title
                                                    , desc = Nothing
                                                    , body = model.compose.body
                                                    }
                                            in
                                            if lowBalance then
                                                Err "Not enough funds."

                                            else
                                                { donateAmount = donateAmount
                                                , author = userInfo.address
                                                , authorBurn = burnAmount
                                                , content = content
                                                , metadata = metadata
                                                }
                                                    |> Ok
                                        )
                                    |> unpack
                                        (\err ->
                                            let
                                                compose =
                                                    model.compose
                                                        |> (\r ->
                                                                { r
                                                                    | inProgress = False
                                                                    , error = Just err
                                                                }
                                                           )
                                            in
                                            ( { model
                                                | compose = compose
                                              }
                                            , GTagData
                                                "post failed"
                                                Nothing
                                                (err
                                                    |> Just
                                                )
                                                Nothing
                                                |> gTagOut
                                            )
                                        )
                                        (\postDraft ->
                                            let
                                                config =
                                                    Chain.getConfig userInfo.chain model.config

                                                txParams =
                                                    postDraft
                                                        |> SSContract.burnEncodedPost userInfo config.contract
                                                        |> Eth.toSend
                                                        |> Eth.encodeSend
                                            in
                                            ( model
                                            , [ Ports.submitPost txParams
                                              , GTagData
                                                    "post tx mining"
                                                    Nothing
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                              ]
                                                |> Cmd.batch
                                            )
                                        )
                            )
                )

        SubmitDraft ->
            ensureUserInfo
                (\userInfo ->
                    let
                        compose =
                            model.compose
                                |> (\r ->
                                        { r
                                            | inProgress = True
                                            , error = Nothing
                                        }
                                   )
                    in
                    ( { model
                        | compose = compose
                      }
                    , SSContract.getEthPriceCmd
                        (Chain.getConfig userInfo.chain model.config)
                        |> Task.attempt PriceResponse
                    )
                )

        SubmitPostTx ->
            ensureUserInfo
                (\userInfo ->
                    model.postState
                        |> unwrap ( model, Cmd.none )
                            (\state ->
                                let
                                    newState =
                                        { state
                                            | inProgress = True
                                            , error = Nothing
                                        }
                                in
                                ( { model
                                    | postState = Just newState
                                  }
                                , SSContract.getEthPriceCmd
                                    (Chain.getConfig userInfo.chain model.config)
                                    |> Task.attempt
                                        (BurnOrTipPriceResponse newState)
                                )
                            )
                )

        BurnOrTipPriceResponse state res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\_ ->
                                let
                                    compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | inProgress = False
                                                        , error = Just "There has been a problem."
                                                    }
                                               )

                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "burn or tip error"
                                            Nothing
                                            (userInfo.address
                                                |> Eth.Utils.addressToString
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | compose = compose
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )
                            )
                            (\price ->
                                state.input
                                    |> Misc.dollarStringToToken price
                                    |> Result.fromMaybe "Invalid tip amount"
                                    |> unpack
                                        (\err ->
                                            let
                                                gtagCmd =
                                                    GTagData
                                                        "burn or tip amount invalid"
                                                        Nothing
                                                        Nothing
                                                        Nothing
                                                        |> gTagOut
                                            in
                                            ( { model
                                                | userNotices = [ UN.unexpectedError err ]
                                              }
                                            , gtagCmd
                                            )
                                        )
                                        (\amount ->
                                            let
                                                config =
                                                    Chain.getConfig userInfo.chain model.config

                                                donation =
                                                    if model.compose.donate then
                                                        TokenValue.div amount 100

                                                    else
                                                        TokenValue.zero

                                                ( fn, tipOrBurn ) =
                                                    case state.showInput of
                                                        Tip ->
                                                            ( SSContract.tipForPost, "tip" )

                                                        Burn ->
                                                            ( SSContract.burnForPost, "burn" )

                                                txParams =
                                                    fn userInfo config.contract state.id.messageHash amount donation
                                                        |> Eth.toSend
                                                        |> Eth.encodeSend

                                                gtagCmd =
                                                    GTagData
                                                        tipOrBurn
                                                        Nothing
                                                        ((state.id.messageHash
                                                            |> Eth.Utils.hexToString
                                                         )
                                                            ++ tipOrBurn
                                                            ++ "ed"
                                                            |> Just
                                                        )
                                                        Nothing
                                                        |> gTagOut
                                            in
                                            ( model
                                            , [ Ports.submitBurnOrTip txParams
                                              , gtagCmd
                                              ]
                                                |> Cmd.batch
                                            )
                                        )
                            )
                )

        DonationCheckboxSet flag ->
            let
                gtagCmd =
                    GTagData
                        "set donation flag"
                        Nothing
                        ((if flag then
                            "True"

                          else
                            "False"
                         )
                            |> Just
                        )
                        Nothing
                        |> gTagOut
            in
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | donate = flag })
              }
            , gtagCmd
            )

        SetPage n ->
            let
                gtagCmd =
                    GTagData
                        "set page"
                        Nothing
                        Nothing
                        (Just n)
                        |> gTagOut
            in
            ( { model
                | currentPage = n
              }
            , gtagCmd
            )

        SetPostInput id val ->
            ( { model
                | postState =
                    { id = id
                    , input = ""
                    , showInput = val
                    , inProgress = False
                    , error = Nothing
                    }
                        |> Just
              }
            , Cmd.none
            )

        CancelPostInput ->
            let
                gtagCmd =
                    GTagData
                        "compose post cancel"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | postState = Nothing
              }
            , gtagCmd
            )

        ChangeDemoPhaceSrc ->
            ( model
            , Random.generate NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
            )

        NewDemoSrc src ->
            ( { model | demoPhaceSrc = src }
            , Cmd.none
            )

        XDaiImport ->
            let
                address =
                    case userInfo model.wallet of
                        Nothing ->
                            "not connected"

                        Just userInfo ->
                            userInfo.address
                                |> Eth.Utils.addressToString

                gtagCmd =
                    GTagData
                        "xdai import clicked"
                        Nothing
                        (address
                            |> Just
                        )
                        Nothing
                        |> gTagOut
            in
            ( { model | chainSwitchInProgress = True }
            , [ Ports.xDaiImport ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        CookieConsentGranted ->
            let
                gtagCmd =
                    GTagData
                        "accept cookies"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | cookieConsentGranted = True
              }
            , [ Ports.consentToCookies ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        ShowNewToSmokeSignalModal flag ->
            let
                ( newGtagHistory, gtagCmd ) =
                    GTagData
                        "show new to smokesignal"
                        Nothing
                        ((if flag == True then
                            "True"

                          else
                            "False"
                         )
                            |> Just
                        )
                        Nothing
                        |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
            in
            ( { model
                | newUserModal = flag
                , gtagHistory = newGtagHistory
              }
            , [ Ports.setVisited ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        SubmitFaucet ->
            ensureUserInfo
                (\userInfo ->
                    let
                        addr =
                            userInfo.address
                                |> Eth.Utils.addressToString
                    in
                    ( { model
                        | compose =
                            model.compose
                                |> (\r ->
                                        { r
                                            | message = Nothing
                                            , error = Nothing
                                        }
                                   )
                        , wallet = Active { userInfo | xDaiStatus = WaitingForApi }
                      }
                    , [ Http.get
                            { url = "https://personal-rxyx.outsystemscloud.com/ERC20FaucetRest/rest/v1/send?In_ReceiverErc20Address=" ++ addr ++ "&In_Token=" ++ model.faucetToken
                            , expect =
                                Http.expectJson
                                    FaucetResponse
                                    Misc.decodeFaucetResponse
                            }
                      , GTagData
                            "faucet request initiated"
                            Nothing
                            Nothing
                            Nothing
                            |> gTagOut
                      ]
                        |> Cmd.batch
                    )
                )

        FaucetResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\e ->
                                ( { model
                                    | compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | message =
                                                            Just "There has been a problem."
                                                    }
                                               )
                                    , wallet = Active { userInfo | xDaiStatus = XDaiStandby }
                                  }
                                , logHttpError "FaucetResponse" e
                                )
                            )
                            (\data ->
                                let
                                    faucetSuccess =
                                        data.status
                                in
                                ( { model
                                    | wallet =
                                        Active
                                            { userInfo
                                                | xDaiStatus =
                                                    if faucetSuccess then
                                                        WaitingForBalance

                                                    else
                                                        XDaiStandby
                                            }
                                    , compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | message =
                                                            if faucetSuccess then
                                                                Just "Your faucet request was successful. Check your wallet for updated balance."

                                                            else
                                                                Just data.message
                                                    }
                                               )
                                  }
                                , if faucetSuccess then
                                    Cmd.batch
                                        [ gTagOut <|
                                            GTagData
                                                "faucet request successful"
                                                Nothing
                                                Nothing
                                                Nothing
                                        , userInfo.address
                                            |> Eth.Utils.addressToString
                                            |> Ports.refreshWallet
                                        ]

                                  else
                                    Cmd.none
                                )
                            )
                )

        TopicSubmit ->
            (if String.isEmpty model.topicInput then
                Misc.defaultTopic

             else
                model.topicInput
            )
                |> Misc.validateTopic
                |> unwrap
                    ( { model
                        | userNotices =
                            [ UN.unexpectedError "Invalid topic" ]
                      }
                    , GTagData
                        "search topic invalid"
                        Nothing
                        (model.topicInput
                            |> Just
                        )
                        Nothing
                        |> gTagOut
                    )
                    (\topic ->
                        ( model
                        , [ pushUrlPathAndUpdateGtagAnalyticsCmd
                                model.navKey
                                (Routing.viewUrlToPathString <| ViewTopic topic)
                          , GTagData
                                "search topic valid"
                                Nothing
                                (topic
                                    |> Just
                                )
                                Nothing
                                |> gTagOut
                          ]
                            |> Cmd.batch
                        )
                    )

        ComposeOpen ->
            if model.wallet == NoneDetected then
                ( { model
                    | compose = { emptyComposeModel | modal = True }
                  }
                , gTagOut <|
                    GTagData
                        "onboarding initiated"
                        Nothing
                        Nothing
                        Nothing
                )

            else
                let
                    topic =
                        model.topicInput
                            |> Misc.validateTopic
                            |> Maybe.withDefault Misc.defaultTopic

                    context =
                        case model.view of
                            ViewTopic t ->
                                Types.TopLevel t

                            ViewPost id ->
                                Types.Reply id

                            _ ->
                                Types.TopLevel topic

                    topicInput =
                        case context of
                            Types.Reply _ ->
                                model.topicInput

                            Types.TopLevel t ->
                                t

                    gtagCmd =
                        if Wallet.isActive model.wallet then
                            GTagData
                                "compose post opened"
                                Nothing
                                Nothing
                                Nothing
                                |> gTagOut

                        else
                            Cmd.none
                in
                ( { model
                    | compose =
                        { emptyComposeModel
                            | modal = True
                            , context = context
                            , title = model.compose.title
                            , body = model.compose.body
                        }
                    , topicInput = topicInput
                  }
                , Cmd.batch
                    [ gtagCmd
                    , model.wallet
                        |> Wallet.userInfo
                        |> unwrap Cmd.none
                            (.address
                                >> Eth.Utils.addressToString
                                >> Ports.refreshWallet
                            )
                    ]
                )

        ComposeClose ->
            let
                gtagCmd =
                    GTagData
                        "compose post closed"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | modal = False
                                    , message = Nothing
                                    , error = Nothing
                                }
                           )
              }
            , gtagCmd
            )

        ComposeBodyChange str ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | body = str })
              }
            , Cmd.none
            )

        ComposeTitleChange str ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | title = str })
              }
            , Cmd.none
            )

        ComposeDollarChange str ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | dollar = str })
              }
            , Cmd.none
            )

        TopicInputChange str ->
            ( { model
                | topicInput = str
              }
            , Cmd.none
            )

        PostInputChange str ->
            ( { model
                | postState =
                    model.postState
                        |> Maybe.map (\r -> { r | input = str })
              }
            , Cmd.none
            )

        SanitizeTopic ->
            ( { model
                | topicInput =
                    model.topicInput
                        |> Misc.validateTopic
                        |> Maybe.withDefault Misc.defaultTopic
              }
            , Cmd.none
            )

        SetSortType newSortType ->
            let
                gtagCmd =
                    GTagData
                        ("change sort type: " ++ sortTypeToString newSortType)
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | sortType = newSortType
                , pages =
                    calculatePagination
                        newSortType
                        model.blockTimes
                        model.accounting
                        model.now
                        model.rootPosts
              }
            , gtagCmd
            )

        SetTooltipState val ->
            ( { model
                | tooltipState =
                    if Just val == model.tooltipState then
                        Nothing

                    else
                        Just val
              }
            , Cmd.none
            )


pushUrlPathAndUpdateGtagAnalyticsCmd : Browser.Navigation.Key -> String -> Cmd Msg
pushUrlPathAndUpdateGtagAnalyticsCmd navKey urlPath =
    Cmd.batch
        [ Browser.Navigation.pushUrl
            navKey
            urlPath
        , Ports.setGtagUrlPath ("/" ++ urlPath)
        ]


handleRoute : Model -> Route -> ( Model, Cmd Msg )
handleRoute model route =
    case route of
        RouteTopics ->
            ( { model
                | view = ViewTopics
              }
            , Cmd.none
            )

        RouteHome ->
            ( { model
                | view = ViewHome
              }
            , Cmd.none
            )

        RouteTxns ->
            ( { model
                | view = ViewTxns
              }
            , Cmd.none
            )

        RouteWallet ->
            ( { model
                | view = ViewWallet
              }
            , Cmd.none
            )

        RouteAbout ->
            ( { model
                | view = ViewAbout
              }
            , Cmd.none
            )

        RouteUser addr ->
            ( { model
                | view = ViewUser addr
              }
            , Cmd.none
            )

        RouteInvalid ->
            ( { model
                | userNotices =
                    [ UN.routeNotFound Nothing ]
              }
            , Cmd.none
            )

        RouteViewPost id ->
            ( { model
                | view = ViewPost id
              }
            , Dict.get (Misc.postIdToKey id) model.rootPosts
                |> Maybe.andThen (.core >> .content >> .desc)
                |> unwrap Cmd.none Ports.setDescription
            )

        RouteTopic topic ->
            topic
                |> Misc.validateTopic
                |> unwrap
                    ( { model
                        | userNotices =
                            [ UN.routeNotFound Nothing ]
                        , view = ViewHome
                      }
                    , Cmd.none
                    )
                    (\t ->
                        ( { model
                            | view = ViewTopic t
                          }
                        , "Discussions related to #"
                            ++ topic
                            ++ " on SmokeSignal"
                            |> Ports.setDescription
                        )
                    )


addPost : LogPost -> Model -> Model
addPost log model =
    case log of
        LogRoot post ->
            let
                rootPosts =
                    model.rootPosts
                        |> Dict.insert post.core.key post
            in
            { model
                | rootPosts = rootPosts
                , topics =
                    model.topics
                        |> Dict.update
                            post.topic
                            (Maybe.withDefault
                                { total = TokenValue.zero
                                , ids = Set.empty
                                }
                                >> Just
                            )
                , pages =
                    rootPosts
                        |> calculatePagination
                            model.sortType
                            model.blockTimes
                            model.accounting
                            model.now
            }

        LogReply post ->
            { model
                | replyPosts =
                    model.replyPosts
                        |> Dict.insert post.core.key post
                , replyIds =
                    model.replyIds
                        |> Dict.update (Misc.postIdToKey post.parent)
                            (Maybe.withDefault Set.empty
                                >> Set.insert post.core.key
                                >> Just
                            )
            }


handleTxReceipt : Chain -> TxReceipt -> ( TxStatus, Maybe LogPost, Maybe UserNotice )
handleTxReceipt chain txReceipt =
    case txReceipt.status of
        Just True ->
            let
                post =
                    txReceipt.logs
                        |> List.map
                            (SSContract.decodePost chain
                                >> .returnData
                            )
                        |> List.filterMap Result.toMaybe
                        |> List.head
            in
            ( post
                |> Maybe.map
                    (\p ->
                        case p of
                            LogReply x ->
                                x.core.id

                            LogRoot x ->
                                x.core.id
                    )
                |> Mined
            , post
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
                UN.unexpectedError "Weird. I Got a transaction receipt with a success value of 'Nothing'. Depending on why this happened I might be a little confused about any mining transactions."
            )


fetchPostInfo : Dict Int Time.Posix -> Config -> Core -> Cmd Msg
fetchPostInfo blockTimes config core =
    [ SSContract.getAccountingCmd
        (Chain.getConfig core.chain config)
        core.id.messageHash
        |> Task.attempt (PostAccountingFetched core.id)
    , if Dict.member core.id.block blockTimes then
        Cmd.none

      else
        Eth.getBlock
            (Chain.getProviderUrl core.chain config)
            core.id.block
            |> Task.map .timestamp
            |> Task.attempt (BlockTimeFetched core.id.block)
    ]
        |> Cmd.batch


logHttpError : String -> Http.Error -> Cmd msg
logHttpError tag =
    Misc.parseHttpError >> (++) (tag ++ ":\n") >> Ports.log


getPostBurnAmount : Float -> String -> Result String TokenValue
getPostBurnAmount price txt =
    if String.isEmpty txt then
        Ok TokenValue.zero

    else
        txt
            |> Misc.dollarStringToToken price
            |> Result.fromMaybe "Invalid burn amount"


calculatePagination : SortType -> Dict Int Time.Posix -> Dict PostKey Accounting -> Time.Posix -> Dict PostKey RootPost -> Array.Array (List PostKey)
calculatePagination sortType blockTimes accounting now =
    Dict.values
        >> List.sortBy
            (.core
                >> Misc.sortPostsFunc sortType blockTimes accounting now
            )
        >> List.map (.core >> .key)
        >> List.Extra.greedyGroupsOf 10
        >> Array.fromList
