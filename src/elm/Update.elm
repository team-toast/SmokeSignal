module Update exposing (update)

import Browser
import Browser.Navigation
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, TxHash)
import Helpers.Element as EH exposing (DisplayProfile(..))
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc exposing (defaultSeoDescription, fetchEthPriceCmd, txInfoToNameStr, updatePublishedPost)
import Ports exposing (connectToWeb3, consentToCookies, gTagOut, setDescription)
import Post
import Random
import Result.Extra exposing (unpack)
import Routing exposing (viewToUrlString)
import Task
import Time
import TokenValue
import Types exposing (..)
import Url
import UserNotice as UN exposing (UserNotice)
import View.Common
import Wallet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg prevModel =
    let
        ensureUserInfo fn =
            prevModel.wallet
                |> Wallet.userInfo
                |> unwrap ( prevModel, Ports.log "Missing wallet" ) fn
    in
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

        RouteChanged route ->
            let
                ( newView, userNotices ) =
                    case route |> Misc.tryRouteToView of
                        Ok v ->
                            ( v, [] )

                        Err err ->
                            ( ViewHome
                            , [ UN.routeNotFound <| Just err ]
                            )
            in
            ( { prevModel
                | view = newView
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
            , Cmd.batch
                [ fetchEthPriceCmd prevModel.config
                , Wallet.userInfo prevModel.wallet
                    |> Maybe.map
                        (\userInfo ->
                            fetchEthBalanceCmd prevModel.config userInfo.address
                        )
                    |> Maybe.withDefault Cmd.none
                ]
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
                |> List.map (Eth.getTxReceipt prevModel.config.httpProviderUrl)
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
                                        ( Types.Active <|
                                            Types.UserInfo
                                                walletSentry.networkId
                                                newAddress
                                                Nothing
                                        , fetchEthBalanceCmd prevModel.config newAddress
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
                    , err
                        |> Json.Decode.errorToString
                        |> (++) "PostLogReceived:\n"
                        |> Ports.log
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
                      --PostId
                      --log.blockNumber
                      --ssPost.hash
                      --}
                      --)
                    , Cmd.batch
                        [ newPostCmd
                        , getBlockTimeIfNeededCmd prevModel.config.httpProviderUrl prevModel.blockTimes log.blockNumber
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

                Err err ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "DAI balance")
                    , logHttpError "PostAccountingFetched" err
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

                    Err err ->
                        ( prevModel
                            |> addUserNotice (UN.web3FetchError "DAI balance")
                        , logHttpError "BalanceFetched" err
                        )

        EthPriceFetched fetchResult ->
            case fetchResult of
                Ok price ->
                    ( { prevModel
                        | ethPrice = Just price
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "ETH price")
                    , logHttpError "EthPriceFetched" err
                    )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err err ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "block time")
                    , logHttpError "BlockTimeFetched" err
                    )

                Ok time ->
                    ( { prevModel
                        | blockTimes =
                            prevModel.blockTimes
                                |> Dict.insert blocknum time
                      }
                    , Cmd.none
                    )

        -- RestoreDraft draft ->
        --     { prevModel
        --         | draftModal = Nothing
        --         --, composeUXModel =
        --         --prevModel.composeUXModel
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
            ( { prevModel
                | userNotices =
                    prevModel.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

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
                            --|> GotoView route
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

        GotoView view ->
            { prevModel
                | view = view
            }
                |> updateSeoDescriptionIfNeededCmd
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Browser.Navigation.pushUrl
                                prevModel.navKey
                                (Routing.viewToUrlString prevModel.basePath view)
                            ]
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
            ( prevModel, Cmd.none )

        --     case prevModel.dProfile of
        --         Desktop ->
        --             ( { prevModel
        --                 | showHalfComposeUX = True
        --                 --, composeUXModel =
        --                 --prevModel.composeUXModel
        --                 -- TODO
        --                 --|> ComposeUX.updateContext composeContext
        --               }
        --             , Cmd.none
        --             )
        --         Mobile ->
        --             prevModel
        --                 |> (GotoView <|
        --                         Routing.Compose composeContext
        --                    )
        -- case prevModel.view of
        --     ViewCompose context ->
        --         prevModel
        --             |> gotoView contextToView
        --     _ ->
        --         ( prevModel, Cmd.none )
        -- ( { prevModel
        --     | showHalfComposeUX = False
        --   }
        -- , Cmd.none
        -- )
        AddUserNotice userNotice ->
            ( prevModel |> addUserNotice userNotice
            , Cmd.none
            )

        SubmitDraft ->
            ensureUserInfo
                (\userInfo ->
                    TokenValue.fromString prevModel.compose.dai
                        |> Result.fromMaybe "Invalid DAI"
                        |> Result.andThen
                            (\burnAmount ->
                                let
                                    donateAmount =
                                        if prevModel.compose.donate then
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
                                        , context =
                                            prevModel.view
                                                |> (\v ->
                                                        case v of
                                                            ViewTopic t ->
                                                                t

                                                            _ ->
                                                                Post.defaultTopic
                                                   )
                                                |> Types.TopLevel
                                        , maybeDecodeError = Nothing
                                        }

                                    content =
                                        { title =
                                            if String.isEmpty prevModel.compose.title then
                                                Nothing

                                            else
                                                Just prevModel.compose.title
                                        , desc = Nothing
                                        , body = prevModel.compose.body
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
                                ( { prevModel
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
                                            |> SSContract.burnEncodedPost prevModel.config.smokeSignalContractAddress
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
                            )
                )

        SubmitPost postDraft ->
            let
                txParams =
                    postDraft
                        |> Misc.encodeDraft
                        |> SSContract.burnEncodedPost prevModel.config.smokeSignalContractAddress
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
                    SSContract.burnForPost prevModel.config.smokeSignalContractAddress postId.messageHash amount prevModel.compose.donate
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
                    SSContract.tipForPost prevModel.config.smokeSignalContractAddress postId.messageHash amount prevModel.compose.donate
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
                | compose =
                    prevModel.compose
                        |> (\r -> { r | donate = flag })
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

        ShowNewToSmokeSignalModal flag ->
            ( { prevModel
                | newUserModal = flag
              }
            , Ports.setVisited ()
            )

        ComposeToggle ->
            ( { prevModel
                | compose =
                    prevModel.compose
                        |> (\r -> { r | modal = not r.modal })
              }
            , Cmd.none
            )

        ComposeBodyChange str ->
            ( { prevModel
                | compose =
                    prevModel.compose
                        |> (\r -> { r | body = str })
              }
            , Cmd.none
            )

        ComposeTitleChange str ->
            ( { prevModel
                | compose =
                    prevModel.compose
                        |> (\r -> { r | title = str })
              }
            , Cmd.none
            )

        ComposeDaiChange str ->
            ( { prevModel
                | compose =
                    prevModel.compose
                        |> (\r -> { r | dai = str })
              }
            , Cmd.none
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


gotoView : View -> Model -> ( Model, Cmd Msg )
gotoView view prevModel =
    { prevModel
        | view = view
    }
        |> updateSeoDescriptionIfNeededCmd


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
                    (case Misc.contextReplyTo publishedPost.core.metadata.context of
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
            prevModel.config
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
                        PostId
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
                UN.unexpectedError "Weird. I Got a transaction receipt with a success value of 'Nothing'. Depending on why this happened I might be a little confused about any mining transactions."
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



-- updateFromPageRoute :
--     Route
--     -> Model
--     -> ( Model, Cmd Msg )
-- updateFromPageRoute route model =
--     if model.route == route then
--         ( model
--         , Cmd.none
--         )
--     else
--         GotoView route model


getBlockTimeIfNeededCmd :
    String
    -> Dict Int Time.Posix
    -> Int
    -> Cmd Msg
getBlockTimeIfNeededCmd httpProviderUrl blockTimes blockNumber =
    if Dict.get blockNumber blockTimes == Nothing then
        getBlockTimeCmd httpProviderUrl blockNumber

    else
        Cmd.none


updateSeoDescriptionIfNeededCmd :
    Model
    -> ( Model, Cmd Msg )
updateSeoDescriptionIfNeededCmd model =
    let
        appropriateMaybeDescription =
            case model.view of
                ViewPost postId ->
                    Misc.getPublishedPostFromId model.publishedPosts postId
                        |> Maybe.andThen (.core >> .content >> .desc)

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
        , setDescription (appropriateMaybeDescription |> Maybe.withDefault defaultSeoDescription)
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


fetchEthPriceCmd : Types.Config -> Cmd Msg
fetchEthPriceCmd config =
    SSContract.getEthPriceCmd
        config
        EthPriceFetched


getBlockTimeCmd : String -> Int -> Cmd Msg
getBlockTimeCmd httpProviderUrl blocknum =
    Eth.getBlock
        httpProviderUrl
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


logHttpError : String -> Http.Error -> Cmd msg
logHttpError tag =
    Misc.parseHttpError >> (++) (tag ++ ":\n") >> Ports.log
