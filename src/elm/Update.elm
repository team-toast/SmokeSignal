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
import Misc exposing (emptyComposeModel)
import Ports
import Post
import Random
import Result.Extra exposing (unpack)
import Routing exposing (viewToUrlString)
import Set
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Url
import UserNotice as UN exposing (UserNotice)
import Wallet


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
                            Browser.Navigation.pushUrl model.navKey (Url.toString url)

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
            ( { model
                | showExpandedTrackedTxs = flag
              }
            , Cmd.none
            )

        RpcResponse res ->
            res
                |> unpack
                    (\e ->
                        ( model, logHttpError "RpcResponse" e )
                    )
                    (\info ->
                        ( { model
                            | wallet = Active info
                            , postState = Nothing
                          }
                        , Cmd.none
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
                                        , Cmd.none
                                        )

                                    Types.OtherErr e ->
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
                                        , Ports.log e
                                        )
                            )
                            (\txHash ->
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
                                  }
                                , Cmd.none
                                )
                            )
                )

        PostTxResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\err ->
                                case err of
                                    Types.UserRejected ->
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
                                        , Cmd.none
                                        )

                                    Types.OtherErr e ->
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
                                        , Ports.log e
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
                                  }
                                , Cmd.none
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
                    ( model, logHttpError "TrackedTxStatusResult" err )

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
                                ( { model
                                    | userNotices = UN.unexpectedError "Please complete the wallet connection process." :: model.userNotices
                                  }
                                , Cmd.none
                                )

                            WalletCancel ->
                                ( { model
                                    | userNotices = UN.unexpectedError "The wallet connection has been cancelled." :: model.userNotices
                                    , wallet = NetworkReady
                                  }
                                , Cmd.none
                                )

                            NetworkNotSupported ->
                                ( { model
                                    | userNotices = UN.unexpectedError "This network is not supported by SmokeSignal." :: model.userNotices
                                    , wallet = NetworkReady
                                  }
                                , Cmd.none
                                )

                            WalletError e ->
                                ( { model
                                    | wallet =
                                        Types.NetworkReady
                                  }
                                , Ports.log e
                                )
                    )
                    (\info ->
                        ( { model
                            | wallet = Active info
                          }
                        , Cmd.none
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
                            case log of
                                LogReply p ->
                                    p.core

                                LogRoot p ->
                                    p.core
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
                                |> calculatePagination model.blockTimes
                                    accounting
                                    model.now
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "accounting data")
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
                    ( model
                    , logHttpError "BalanceFetched" err
                    )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "block time")
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
            , Browser.Navigation.pushUrl
                model.navKey
                (Routing.viewToUrlString view)
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

        AddUserNotice userNotice ->
            ( model |> addUserNotice userNotice
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
                                            ( { model
                                                | userNotices = [ UN.unexpectedError err ]
                                              }
                                            , Cmd.none
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
                                            , Ports.submitPost txParams
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
                    ( { model | compose = compose }
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
                                ( { model | postState = Just newState }
                                , SSContract.getEthPriceCmd
                                    (Chain.getConfig userInfo.chain model.config)
                                    |> Task.attempt (PostTxPriceResponse newState)
                                )
                            )
                )

        PostTxPriceResponse state res ->
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
                                            "[Event]"
                                            Nothing
                                            ("error "
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model | compose = compose }, Cmd.none )
                            )
                            (\price ->
                                state.input
                                    |> Misc.dollarStringToToken price
                                    |> Result.fromMaybe "Invalid tip amount"
                                    |> unpack
                                        (\err ->
                                            ( { model
                                                | userNotices = [ UN.unexpectedError err ]
                                              }
                                            , Cmd.none
                                            )
                                        )
                                        (\amount ->
                                            let
                                                config =
                                                    Chain.getConfig userInfo.chain model.config

                                                fn =
                                                    case state.showInput of
                                                        Tip ->
                                                            SSContract.tipForPost

                                                        Burn ->
                                                            SSContract.burnForPost

                                                txParams =
                                                    fn userInfo config.contract state.id.messageHash amount model.compose.donate
                                                        |> Eth.toSend
                                                        |> Eth.encodeSend
                                            in
                                            ( model, Ports.txOut txParams )
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
            ( { model
                | currentPage = n
              }
            , Cmd.none
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
            ( model
            , Ports.xDaiImport ()
            )

        CookieConsentGranted ->
            ( { model
                | cookieConsentGranted = True
              }
            , Cmd.batch
                [ Ports.consentToCookies ()
                , gTagOut <|
                    GTagData
                        "accept cookies"
                        Nothing
                        Nothing
                        Nothing
                ]
            )

        ShowNewToSmokeSignalModal flag ->
            ( { model
                | newUserModal = flag
              }
            , Ports.setVisited ()
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
                        , expect = Http.expectWhatever FaucetResponse
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
                                [ UN.unexpectedError "There has been a problem." ]
                          }
                        , logHttpError "FaucetResponse" e
                        )
                    )
                    (\info ->
                        ( { model
                            | userNotices =
                                [ UN.notify "Your faucet request was successful." ]
                            , faucetInProgress = False
                          }
                        , Cmd.none
                        )
                    )

        TopicSubmit ->
            (if String.isEmpty model.topicInput then
                Post.defaultTopic

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
                        , Browser.Navigation.pushUrl
                            model.navKey
                            (Routing.viewToUrlString <| ViewTopic topic)
                        )
                    )

        ComposeOpen ->
            let
                topic =
                    model.topicInput
                        |> Misc.validateTopic
                        |> Maybe.withDefault Post.defaultTopic

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
            in
            ( { model
                | compose = { emptyComposeModel | modal = True, context = context }
                , topicInput = topicInput
              }
            , Cmd.none
            )

        ComposeClose ->
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | modal = False
                                }
                           )
              }
            , Cmd.none
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
                        |> Maybe.withDefault Post.defaultTopic
              }
            , Cmd.none
            )

        GoBack ->
            ( model
              -- To prevent navigating to a previous website
            , if model.hasNavigated then
                Browser.Navigation.back model.navKey 1

              else
                Browser.Navigation.pushUrl
                    model.navKey
                    (Routing.viewToUrlString ViewHome)
            )


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

        RouteMalformedPostId ->
            ( { model
                | userNotices =
                    [ UN.routeNotFound Nothing ]
              }
            , Cmd.none
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
                        |> calculatePagination model.blockTimes
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


calculatePagination : Dict Int Time.Posix -> Dict PostKey Accounting -> Time.Posix -> Dict PostKey RootPost -> Array.Array (List PostKey)
calculatePagination blockTimes accounting now =
    Dict.values
        >> List.sortBy
            (.core
                >> Misc.sortPosts blockTimes accounting now
            )
        >> List.map (.core >> .key)
        >> List.Extra.greedyGroupsOf 10
        >> Array.fromList
