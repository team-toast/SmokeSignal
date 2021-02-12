module Update exposing (update)

import Browser
import Browser.Navigation
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Sentry.Event as EventSentry
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address)
import Eth.Utils
import GTag exposing (GTagData, gTagOut)
import Helpers.Element as EH exposing (DisplayProfile(..))
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc exposing (defaultSeoDescription, txInfoToNameStr)
import Ports
import Post
import Random
import Result.Extra exposing (unpack)
import Routing exposing (viewToUrlString)
import Set
import Task
import Time
import TokenValue
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

                RouteViewTopic topic ->
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

        Tick newTime ->
            ( { model | now = newTime }, Cmd.none )

        Resize width _ ->
            ( { model
                | dProfile =
                    EH.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        EveryFewSeconds ->
            ( model
            , Cmd.batch
                [ SSContract.getEthPriceCmd
                    model.config
                    EthPriceFetched
                , Wallet.userInfo model.wallet
                    |> Maybe.map
                        (\userInfo ->
                            fetchEthBalanceCmd model.config userInfo.address
                        )
                    |> Maybe.withDefault Cmd.none
                ]
            )

        ShowExpandedTrackedTxs flag ->
            ( { model
                | showExpandedTrackedTxs = flag
              }
            , Cmd.none
            )

        CheckTrackedTxsStatus ->
            ( model
            , model.trackedTxs
                |> Dict.values
                |> List.filter
                    (\trackedTx ->
                        trackedTx.status == Mining
                    )
                |> List.map .txHash
                |> List.map (Eth.getTxReceipt model.config.httpProviderUrl)
                |> List.map (Task.attempt TrackedTxStatusResult)
                |> Cmd.batch
            )

        TrackedTxStatusResult txReceiptResult ->
            case txReceiptResult of
                Err errStr ->
                    -- Hasn't yet been mined; make no change
                    ( model, Cmd.none )

                Ok txReceipt ->
                    let
                        ( newStatus, maybePublishedPost, maybeUserNotice ) =
                            handleTxReceipt txReceipt

                        newModel =
                            { model
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
                    in
                    maybePublishedPost
                        |> unwrap ( newModel, Cmd.none )
                            (addPost newModel)

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        ( newWallet, cmd ) =
                            case walletSentry.account of
                                Just newAddress ->
                                    if (model.wallet |> Wallet.userInfo |> Maybe.map .address) == Just newAddress then
                                        ( model.wallet
                                        , Cmd.none
                                        )

                                    else
                                        ( Types.Active <|
                                            Types.UserInfo
                                                walletSentry.networkId
                                                newAddress
                                                Nothing
                                        , fetchEthBalanceCmd model.config newAddress
                                        )

                                Nothing ->
                                    ( Types.OnlyNetwork walletSentry.networkId
                                    , Cmd.none
                                    )
                    in
                    ( { model
                        | wallet = newWallet
                      }
                    , cmd
                    )

                Err errStr ->
                    ( model |> addUserNotice (UN.walletError errStr)
                    , Cmd.none
                    )

        TxSentryMsg subMsg ->
            let
                ( newTxSentry, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( { model | txSentry = newTxSentry }, subCmd )

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        model.eventSentry
            in
            ( { model
                | eventSentry =
                    newEventSentry
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
                    addPost model log

        PostAccountingFetched postId res ->
            case res of
                Ok accounting ->
                    let
                        key =
                            Misc.postIdToKey postId

                        maybeTopic =
                            model.rootPosts
                                |> Dict.get key
                                |> Maybe.map .topic

                        updateTopics =
                            maybeTopic
                                |> unwrap identity
                                    (\topic ->
                                        Dict.update
                                            topic
                                            (unwrap accounting.totalBurned
                                                (TokenValue.add
                                                    accounting.totalBurned
                                                )
                                                >> Just
                                            )
                                    )
                    in
                    ( { model
                        | accounting =
                            model.accounting
                                |> Dict.insert key accounting
                        , topics =
                            model.topics
                                |> updateTopics
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "DAI balance")
                    , logHttpError "PostAccountingFetched" err
                    )

        BalanceFetched address fetchResult ->
            let
                maybeCurrentAddress =
                    Wallet.userInfo model.wallet
                        |> Maybe.map .address
            in
            if maybeCurrentAddress /= Just address then
                ( model, Cmd.none )

            else
                case fetchResult of
                    Ok balance ->
                        let
                            newWallet =
                                model.wallet |> Wallet.withFetchedBalance balance
                        in
                        ( { model
                            | wallet = newWallet
                          }
                        , Cmd.none
                        )

                    Err err ->
                        ( model
                            |> addUserNotice (UN.web3FetchError "DAI balance")
                        , logHttpError "BalanceFetched" err
                        )

        EthPriceFetched fetchResult ->
            case fetchResult of
                Ok price ->
                    ( { model
                        | ethPrice = price
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "ETH price")
                    , logHttpError "EthPriceFetched" err
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

        -- RestoreDraft draft ->
        --     { model
        --         | draftModal = Nothing
        --         --, composeUXModel =
        --         --model.composeUXModel
        --         --|> (\composeUXModel ->
        --         --{ composeUXModel
        --         --| content = draft.core.content
        --         --, daiInput =
        --         --draft.core.authorBurn
        --         --|> TokenValue.toFloatString Nothing
        --         --}
        --         --)
        --         -- TODO
        --         --|> identity
        --     }
        --         |> (GotoView <| ViewCompose draft.core.metadata.context)
        DismissNotice id ->
            ( { model
                | userNotices =
                    model.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        TxSigned txInfo res ->
            case res of
                Ok txHash ->
                    ( { model
                        | showExpandedTrackedTxs = True
                        , trackedTxs =
                            model.trackedTxs
                                |> Dict.insert (Eth.Utils.txHashToString txHash)
                                    { txHash = txHash
                                    , txInfo = txInfo
                                    , status = Mining
                                    }
                      }
                    , Cmd.none
                    )

                Err errStr ->
                    ( model
                        |> addUserNotice
                            (UN.web3SigError
                                (txInfoToNameStr txInfo)
                                errStr
                            )
                    , Cmd.none
                    )

        GotoView view ->
            ( model
            , Browser.Navigation.pushUrl
                model.navKey
                (Routing.viewToUrlString view)
            )

        ConnectToWeb3 ->
            case model.wallet of
                Types.NoneDetected ->
                    ( model |> addUserNotice UN.cantConnectNoWeb3
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Ports.connectToWeb3 ()
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

        StartInlineCompose composeContext ->
            ( model, Cmd.none )

        --     case model.dProfile of
        --         Desktop ->
        --             ( { model
        --                 | showHalfComposeUX = True
        --                 --, composeUXModel =
        --                 --model.composeUXModel
        --                 -- TODO
        --                 --|> ComposeUX.updateContext composeContext
        --               }
        --             , Cmd.none
        --             )
        --         Mobile ->
        --             model
        --                 |> (GotoView <|
        --                         Routing.Compose composeContext
        --                    )
        -- case model.view of
        --     ViewCompose context ->
        --         model
        --             |> gotoView contextToView
        --     _ ->
        --         ( model, Cmd.none )
        -- ( { model
        --     | showHalfComposeUX = False
        --   }
        -- , Cmd.none
        -- )
        AddUserNotice userNotice ->
            ( model |> addUserNotice userNotice
            , Cmd.none
            )

        SubmitDraft ->
            ensureUserInfo
                (\userInfo ->
                    model.compose.dollar
                        |> Misc.dollarStringToToken model.ethPrice
                        |> Result.fromMaybe "Invalid input"
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
                                            (userInfo.balance
                                                |> Maybe.withDefault TokenValue.zero
                                            )
                                            /= LT

                                    metadata =
                                        { metadataVersion =
                                            Post.currentMetadataVersion
                                        , context = model.compose.context
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
                                    , core =
                                        { author = userInfo.address
                                        , authorBurn = burnAmount
                                        , content = content
                                        , metadata = metadata
                                        }
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
                                    txParams =
                                        postDraft
                                            |> Misc.encodeDraft
                                            |> SSContract.burnEncodedPost model.config.smokeSignalContractAddress
                                            |> Eth.toSend

                                    listeners =
                                        { onMined = Nothing
                                        , onSign = Just <| TxSigned <| PostTx postDraft
                                        , onBroadcast = Nothing
                                        }

                                    ( txSentry, cmd ) =
                                        TxSentry.customSend model.txSentry listeners txParams
                                in
                                ( { model
                                    | txSentry = txSentry
                                  }
                                , cmd
                                )
                            )
                )

        SubmitBurn postId ->
            model.compose.dollar
                |> Misc.dollarStringToToken model.ethPrice
                |> unwrap
                    ( { model
                        | userNotices = [ UN.unexpectedError "Invalid input" ]
                      }
                    , Cmd.none
                    )
                    (\amount ->
                        let
                            txParams =
                                SSContract.burnForPost model.config.smokeSignalContractAddress postId.messageHash amount model.compose.donate
                                    |> Eth.toSend

                            listeners =
                                { onMined = Nothing
                                , onSign = Just <| TxSigned <| BurnTx postId amount
                                , onBroadcast = Nothing
                                }

                            ( txSentry, cmd ) =
                                TxSentry.customSend model.txSentry listeners txParams
                        in
                        ( { model
                            | txSentry = txSentry
                          }
                        , cmd
                        )
                    )

        SubmitTip postId ->
            model.compose.dollar
                |> Misc.dollarStringToToken model.ethPrice
                |> unwrap
                    ( { model
                        | userNotices = [ UN.unexpectedError "Invalid input" ]
                      }
                    , Cmd.none
                    )
                    (\amount ->
                        let
                            txParams =
                                SSContract.tipForPost model.config.smokeSignalContractAddress postId.messageHash amount model.compose.donate
                                    |> Eth.toSend

                            listeners =
                                { onMined = Nothing
                                , onSign = Just <| TxSigned <| TipTx postId amount
                                , onBroadcast = Nothing
                                }

                            ( txSentry, cmd ) =
                                TxSentry.customSend model.txSentry listeners txParams
                        in
                        ( { model
                            | txSentry = txSentry
                            , tipOpen = Nothing
                          }
                        , cmd
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

        ViewDraft maybeDraft ->
            ( { model
                | draftModal = maybeDraft
              }
            , Cmd.none
            )

        SetTipOpen state ->
            ( { model
                | tipOpen = Just state
              }
            , Cmd.none
            )

        CancelTipOpen ->
            ( { model
                | tipOpen = Nothing
              }
            , Cmd.none
            )

        ChangeDemoPhaceSrc ->
            ( model
              --, Random.generate MutateDemoSrcWith mutateInfoGenerator
            , Random.generate NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
            )

        NewDemoSrc src ->
            ( { model | demoPhaceSrc = src }
            , Cmd.none
            )

        ClickHappened ->
            ( { model
                | showAddressId = Nothing
                , showExpandedTrackedTxs = False
                , draftModal = Nothing
              }
            , Cmd.none
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

        ComposeOpen ->
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | modal = True
                                    , title = ""
                                    , body = ""
                                    , dollar = ""
                                    , context =
                                        case model.view of
                                            ViewTopic t ->
                                                Types.TopLevel t

                                            ViewPost id ->
                                                Types.Reply id

                                            _ ->
                                                model.topicInput
                                                    |> Misc.validateTopic
                                                    |> Maybe.withDefault Post.defaultTopic
                                                    |> Types.TopLevel
                                }
                           )
                , topicInput = ""
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


addPost : Model -> LogPost -> ( Model, Cmd Msg )
addPost model log =
    case log of
        LogRoot post ->
            ( { model
                | rootPosts =
                    model.rootPosts
                        |> Dict.insert post.core.key post
                , topics =
                    model.topics
                        |> Dict.update
                            post.topic
                            (Maybe.withDefault TokenValue.zero
                                >> Just
                            )
              }
            , fetchPostInfo model.blockTimes model.config post.core.id
            )

        LogReply post ->
            ( { model
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
            , fetchPostInfo model.blockTimes model.config post.core.id
            )


handleTxReceipt :
    Eth.Types.TxReceipt
    -> ( TxStatus, Maybe LogPost, Maybe UserNotice )
handleTxReceipt txReceipt =
    case txReceipt.status of
        Just True ->
            let
                post =
                    txReceipt.logs
                        |> List.map (SSContract.decodePost >> .returnData)
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


fetchPostInfo : Dict Int Time.Posix -> Config -> PostId -> Cmd Msg
fetchPostInfo blockTimes config id =
    [ SSContract.getAccountingCmd
        config
        id.messageHash
        |> Task.attempt (PostAccountingFetched id)
    , if Dict.member id.block blockTimes then
        Cmd.none

      else
        Eth.getBlock
            config.httpProviderUrl
            id.block
            |> Task.map .timestamp
            |> Task.attempt (BlockTimeFetched id.block)
    ]
        |> Cmd.batch


updateSeoDescriptionIfNeededCmd :
    Model
    -> ( Model, Cmd Msg )
updateSeoDescriptionIfNeededCmd model =
    let
        appropriateMaybeDescription =
            case model.view of
                ViewPost postId ->
                    --Misc.getPublishedPostFromId model.publishedPosts postId
                    --|> Maybe.andThen (.core >> .content >> .desc)
                    Nothing

                ViewTopic topic ->
                    Just <| "Discussions related to #" ++ topic ++ " on SmokeSignal"

                _ ->
                    Nothing

        -- Nothing
    in
    if appropriateMaybeDescription /= model.maybeSeoDescription then
        ( { model
            | maybeSeoDescription = appropriateMaybeDescription
          }
        , Ports.setDescription (appropriateMaybeDescription |> Maybe.withDefault defaultSeoDescription)
        )

    else
        ( model, Cmd.none )


fetchEthBalanceCmd : Types.Config -> Address -> Cmd Msg
fetchEthBalanceCmd config address =
    Eth.getBalance
        config.httpProviderUrl
        address
        |> Task.map TokenValue.tokenValue
        |> Task.attempt (BalanceFetched address)


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
