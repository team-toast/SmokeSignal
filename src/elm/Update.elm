module Update exposing (update)

import Array
import Browser
import Browser.Navigation
import Chain
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Sentry.Event as EventSentry
import Eth.Types exposing (TxReceipt)
import Eth.Utils
import GTag exposing (GTagData, gTagOut, gTagOutOnlyOnLabelOrValueChange, gTagOutOnlyOnceForEvent)
import Helpers.Element as EH exposing (DisplayProfile(..))
import Http
import Json.Decode
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc exposing (emptyComposeModel, sortTypeToString)
import Ports
import Post
import Random
import Result.Extra exposing (unpack)
import Routing exposing (viewUrlToPathString)
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
            handleRoute { model | hasNavigated = True } route

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

        RpcResponse res ->
            res
                |> unpack
                    (\e ->
                        let
                            gtagCmd =
                                GTagData
                                    "rpc response"
                                    ("Error"
                                        |> Just
                                    )
                                    (e
                                        |> Misc.parseHttpError
                                        |> Just
                                    )
                                    Nothing
                                    |> gTagOut
                        in
                        ( model
                        , [ logHttpError "RpcResponse" e
                          , gtagCmd
                          ]
                            |> Cmd.batch
                        )
                    )
                    (\info ->
                        let
                            gtagCmd =
                                GTagData
                                    "rpc response"
                                    ("Success"
                                        |> Just
                                    )
                                    Nothing
                                    Nothing
                                    |> gTagOut
                        in
                        ( { model
                            | wallet = Active info
                            , postState = Nothing
                          }
                        , gtagCmd
                        )
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
                                                    "post response"
                                                    Nothing
                                                    (Just "transaction cancelled.")
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
                                                    "post response"
                                                    Nothing
                                                    (Just "there was a problem")
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
                                            "post response"
                                            (Just "transaction mining")
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
                                                , txInfo = PostTx
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
                                                    "post response"
                                                    Nothing
                                                    (Just "transaction cancelled.")
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
                                                    "post response"
                                                    Nothing
                                                    (Just "there was a problem")
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
                                                                TipTx state.id
                                                    , status = Mining
                                                    , chain = userInfo.chain
                                                    }
                                                )

                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "post response"
                                            (Just "transaction mining")
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
                                                        PostTx ->
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

                                                        BurnTx id ->
                                                            Misc.getPostOrReply id model
                                                    )
                                                        |> unwrap Cmd.none
                                                            (fetchPostInfo model.blockTimes model.config)
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
                                              }
                                            , if isMined then
                                                fetchAccounting

                                              else
                                                Cmd.none
                                            )
                                        )
                            )

        WalletResponse res ->
            res
                |> unpack
                    (\err ->
                        case err of
                            WalletInProgress ->
                                let
                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "wallet response"
                                            (Just "error")
                                            (Just "connection incomplete")
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | userNotices = UN.unexpectedError "Please complete the wallet connection process." :: model.userNotices
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )

                            WalletCancel ->
                                let
                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "wallet response"
                                            (Just "error")
                                            (Just "connection cancelled")
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | userNotices = UN.unexpectedError "The wallet connection has been cancelled." :: model.userNotices
                                    , wallet = NetworkReady
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )

                            NetworkNotSupported ->
                                let
                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "wallet response"
                                            (Just "error")
                                            (Just "network not supported")
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | userNotices = UN.unexpectedError "This network is not supported by SmokeSignal." :: model.userNotices
                                    , wallet = NetworkReady
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )

                            WalletError e ->
                                let
                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "wallet response"
                                            (Just "error")
                                            (e
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | wallet =
                                        Types.NetworkReady
                                    , gtagHistory = newGtagHistory
                                    , chainSwitchInProgress = False
                                  }
                                , [ Ports.log e
                                  , gtagCmd
                                  ]
                                    |> Cmd.batch
                                )
                    )
                    (\info ->
                        let
                            ( newGtagHistory, gtagCmd ) =
                                GTagData
                                    "wallet response"
                                    (Just "success")
                                    (Just "connected")
                                    Nothing
                                    |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory

                            onboardComplete =
                                info.chain == XDai && not (TokenValue.isZero info.balance)
                        in
                        ( { model
                            | wallet = Active info
                            , gtagHistory = newGtagHistory
                            , hasOnboarded = onboardComplete || model.hasOnboarded
                            , chainSwitchInProgress = False
                          }
                        , [ gtagCmd
                          , if onboardComplete then
                                Ports.setOnboarded ()

                            else
                                Cmd.none
                          ]
                            |> Cmd.batch
                        )
                    )

        EventSentryMsg chain eventMsg ->
            case chain of
                Eth ->
                    let
                        ( newEventSentry, cmd ) =
                            EventSentry.update
                                eventMsg
                                model.sentries.ethereum
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
                            EventSentry.update
                                eventMsg
                                model.sentries.xDai
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
                    let
                        gtagCmd =
                            GTagData
                                "post log received"
                                (Just "error")
                                (err
                                    |> Json.Decode.errorToString
                                    |> Just
                                )
                                Nothing
                                |> gTagOut
                    in
                    ( model
                    , [ err
                            |> Json.Decode.errorToString
                            |> String.left 200
                            |> (++) "PostLogReceived:\n"
                            |> Ports.log
                      , gtagCmd
                      ]
                        |> Cmd.batch
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
                        ( core, replyOrPost ) =
                            case log of
                                LogReply p ->
                                    ( p.core, "reply" )

                                LogRoot p ->
                                    ( p.core, "post" )

                        gtagCmd =
                            GTagData
                                "post log received"
                                (Just "success")
                                (Just replyOrPost)
                                Nothing
                                |> gTagOut
                    in
                    ( addPost log model
                    , [ fetchPostInfo model.blockTimes model.config core
                      , gtagCmd
                      ]
                        |> Cmd.batch
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

        BalanceFetched address res ->
            case res of
                Ok balance ->
                    let
                        ( newGtagHistory, gtagCmd ) =
                            GTagData
                                "balance fetched"
                                Nothing
                                (Just <| TokenValue.toFloatString Nothing balance)
                                Nothing
                                |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                    in
                    ( { model
                        | wallet =
                            model.wallet
                                |> Wallet.userInfo
                                |> unwrap
                                    model.wallet
                                    (\userInfo ->
                                        Active
                                            { userInfo
                                                | balance =
                                                    if userInfo.address == address then
                                                        balance

                                                    else
                                                        userInfo.balance
                                            }
                                    )
                        , gtagHistory = newGtagHistory
                      }
                    , gtagCmd
                    )

                Err err ->
                    let
                        gtagCmd =
                            GTagData
                                "balance fetched"
                                Nothing
                                (Just "error")
                                Nothing
                                |> gTagOut
                    in
                    ( model
                    , [ logHttpError "BalanceFetched" err
                      , gtagCmd
                      ]
                        |> Cmd.batch
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
            ( model
            , pushUrlPathAndUpdateGtagAnalyticsCmd
                model.navKey
                (Routing.viewUrlToPathString view)
            )

        ConnectToWeb3 ->
            let
                ( newGtagHistory, gtagCmd ) =
                    GTagData
                        "Web3 Connected"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOutOnlyOnceForEvent model.gtagHistory
            in
            ( { model | wallet = Connecting, gtagHistory = newGtagHistory }
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

                                    gtagCmd =
                                        GTagData
                                            "price response"
                                            (Just "error")
                                            (userInfo.address
                                                |> Eth.Utils.addressToString
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOut
                                in
                                ( { model | compose = compose }, gtagCmd )
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
                                                    TokenValue.compare
                                                        (TokenValue.add burnAmount donateAmount)
                                                        userInfo.balance
                                                        /= LT

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

                                                gtagCmd =
                                                    GTagData
                                                        "price response"
                                                        (Just "error")
                                                        ("unexpected error "
                                                            ++ err
                                                            |> Just
                                                        )
                                                        Nothing
                                                        |> gTagOut
                                            in
                                            ( { model
                                                | compose = compose
                                              }
                                            , gtagCmd
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

                                                gtagCmd =
                                                    GTagData
                                                        "price response"
                                                        (Just "success")
                                                        (Just "post draft")
                                                        Nothing
                                                        |> gTagOut
                                            in
                                            ( model
                                            , [ Ports.submitPost txParams
                                              , gtagCmd
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

                        ( newGtagHistory, gtagCmd ) =
                            GTagData
                                "submit draft"
                                Nothing
                                ("draft submitted "
                                    ++ Eth.Utils.addressToString userInfo.address
                                    |> Just
                                )
                                Nothing
                                |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                    in
                    ( { model
                        | compose = compose
                        , gtagHistory = newGtagHistory
                      }
                    , [ SSContract.getEthPriceCmd
                            (Chain.getConfig userInfo.chain model.config)
                            |> Task.attempt PriceResponse
                      , gtagCmd
                      ]
                        |> Cmd.batch
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

                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "submit post"
                                            (Just "success")
                                            ("post submitted "
                                                ++ Eth.Utils.addressToString userInfo.address
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | postState = Just newState
                                    , gtagHistory = newGtagHistory
                                  }
                                , [ SSContract.getEthPriceCmd
                                        (Chain.getConfig userInfo.chain model.config)
                                        |> Task.attempt
                                            (BurnOrTipPriceResponse newState)
                                  , gtagCmd
                                  ]
                                    |> Cmd.batch
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
                                            "post price response"
                                            ("error "
                                                |> Just
                                            )
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
                                                        "post price response"
                                                        (Just "error")
                                                        (Just "invalid tip amount")
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

                                                fn =
                                                    case state.showInput of
                                                        Tip ->
                                                            SSContract.tipForPost

                                                        Burn ->
                                                            SSContract.burnForPost

                                                txParams =
                                                    fn userInfo config.contract state.id.messageHash amount donation
                                                        |> Eth.toSend
                                                        |> Eth.encodeSend

                                                gtagCmd =
                                                    GTagData
                                                        "post price response"
                                                        (Just "success")
                                                        ((state.id.messageHash
                                                            |> Eth.Utils.hexToString
                                                         )
                                                            ++ " donated"
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
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | donate = flag })
              }
            , Cmd.none
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
            ( { model
                | postState = Nothing
              }
            , Cmd.none
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
                        "xdai import"
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
                        "show modal"
                        (Just "new to smokesignal")
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
                    ( { model | faucetInProgress = True }
                    , Http.get
                        { url = "https://personal-rxyx.outsystemscloud.com/ERC20FaucetRest/rest/v1/send?In_ReceiverErc20Address=" ++ addr ++ "&In_Token=" ++ model.faucetToken
                        , expect =
                            Http.expectJson
                                FaucetResponse
                                Misc.decodeFaucetResponse
                        }
                    )
                )

        FaucetResponse res ->
            res
                |> unpack
                    (\e ->
                        ( { model
                            | faucetInProgress = False
                            , userNotices =
                                model.userNotices
                                    |> List.append
                                        [ UN.unexpectedError "There has been a problem." ]
                          }
                        , logHttpError "FaucetResponse" e
                        )
                    )
                    (\data ->
                        ( { model
                            | userNotices =
                                model.userNotices
                                    |> List.append
                                        [ if data.status then
                                            UN.notify "Your faucet request was successful."

                                          else
                                            UN.notify data.message
                                        ]
                            , faucetInProgress = False
                            , hasOnboarded = True
                          }
                        , Ports.setOnboarded ()
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
                    , Cmd.none
                    )
                    (\topic ->
                        ( model
                        , pushUrlPathAndUpdateGtagAnalyticsCmd
                            model.navKey
                            (Routing.viewUrlToPathString <| ViewTopic topic)
                        )
                    )

        ComposeOpen ->
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

                address =
                    case userInfo model.wallet of
                        Nothing ->
                            "not connected"

                        Just userInfo ->
                            userInfo.address
                                |> Eth.Utils.addressToString

                gtagCmd =
                    GTagData
                        "show modal"
                        (Just "compose post")
                        (address
                            |> Just
                        )
                        Nothing
                        |> gTagOut
            in
            ( { model
                | compose = { emptyComposeModel | modal = True, context = context }
                , topicInput = topicInput
              }
            , gtagCmd
            )

        ComposeClose ->
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
                        "close modal"
                        (Just "compose post")
                        (address
                            |> Just
                        )
                        Nothing
                        |> gTagOut
            in
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | modal = False
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

        GoBack ->
            ( model
              -- To prevent navigating to a previous website
            , if model.hasNavigated then
                Browser.Navigation.back model.navKey 1

              else
                pushUrlPathAndUpdateGtagAnalyticsCmd
                    model.navKey
                    (Routing.viewUrlToPathString ViewHome)
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
