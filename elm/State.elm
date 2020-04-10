port module State exposing (init, subscriptions, update)

import Browser
import Browser.Events
import Browser.Navigation
import CommonTypes exposing (..)
import Config
import Contracts.Dai as Dai
import Contracts.SmokeSignal as SSContract
import Dict exposing (Dict)
import Eth
import Eth.Decode
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry
import Eth.Types exposing (Address)
import Eth.Utils
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import MaybeDebugLog exposing (maybeDebugLog)
import Message exposing (Message)
import Routing exposing (Route)
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
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
            fetchMessagesFromBlockrangeCmd
                (Eth.Types.BlockNum Config.startScanBlock)
                Eth.Types.LatestBlock
                initEventSentry
    in
    { navKey = key
    , wallet = wallet
    , now = Time.millisToPosix flags.nowInMillis
    , route = Routing.InitialBlank
    , txSentry = txSentry
    , eventSentry = eventSentry
    , messages = Dict.empty
    , replies = []
    , miningMessages = Dict.empty
    , showComposeUX = False
    , composeUXModel = initialComposeUXModel
    , blockTimes = Dict.empty
    , showAddress = Nothing
    , userNotices = walletNotices
    , viewFilter = None
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


initialComposeUXModel : ComposeUXModel
initialComposeUXModel =
    { message = ""
    , daiInput = ""
    , donateChecked = True
    , miningUnlockTx = Nothing
    , metadata = Message.noMetadata
    }


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
            prevModel |> updateFromPageRoute (url |> Routing.urlToRoute)

        GotoRoute route ->
            prevModel
                |> gotoRoute route
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Browser.Navigation.pushUrl
                                prevModel.navKey
                                (Routing.routeToString route)
                            ]
                    )

        Tick newTime ->
            ( { prevModel | now = newTime }, Cmd.none )

        EveryFewSeconds ->
            ( prevModel
            , Wallet.userInfo prevModel.wallet
                |> Maybe.map
                    (\userInfo ->
                        fetchDaiBalanceAndAllowanceCmd userInfo.address
                    )
                |> Maybe.withDefault Cmd.none
            )

        CheckMiningMessagesStatus ->
            ( prevModel
            , Dict.keys prevModel.miningMessages
                |> List.map Eth.Utils.unsafeToTxHash
                |> List.map (Eth.getTxReceipt Config.httpProviderUrl)
                |> List.map (Task.attempt MiningMessageStatusResult)
                |> Cmd.batch
            )

        MiningMessageStatusResult txReceiptResult ->
            case txReceiptResult of
                Err _ ->
                    -- Hasn't yet been mined; make no change
                    ( prevModel, Cmd.none )

                Ok txReceipt ->
                    let
                        txHashStr =
                            txReceipt.hash |> Eth.Utils.txHashToString

                        maybeDraft =
                            prevModel.miningMessages
                                |> Dict.get txHashStr
                                |> Maybe.map .draft

                        maybeSsMessage =
                            txReceipt.logs
                                |> List.filter
                                    (\log ->
                                        List.head log.topics == Just Config.messageBurnEventSig
                                    )
                                |> List.head
                                |> Maybe.map (Eth.Decode.event SSContract.messageBurnDecoder)
                                |> Maybe.andThen (.returnData >> Result.toMaybe)
                    in
                    case ( maybeDraft, maybeSsMessage ) of
                        ( Nothing, _ ) ->
                            -- No matching draft found in miningMessages; ignore
                            ( prevModel, Cmd.none )

                        ( _, Nothing ) ->
                            ( prevModel
                                |> addUserNotice
                                    (UN.unexpectedError "Unexpected error when checking mining message status. Your message may have failed to mine." Nothing)
                            , Cmd.none
                            )

                        ( Just draft, Just ssMessage ) ->
                            ( prevModel
                                |> addMessage txReceipt.blockNumber
                                    (Message.fromContractEvent
                                        txReceipt.blockNumber
                                        ssMessage
                                    )
                                |> removeMiningMessage txHashStr
                            , getBlockTimeIfNeededCmd prevModel.blockTimes txReceipt.blockNumber
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
                                                Nothing
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
                    , Cmd.none
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

        MessageLogReceived log ->
            let
                decodedEventLog =
                    Eth.Decode.event SSContract.messageBurnDecoder log
            in
            case decodedEventLog.returnData of
                Err err ->
                    ( prevModel |> addUserNotice (UN.eventDecodeError err)
                    , Cmd.none
                    )

                Ok ssMessage ->
                    ( prevModel
                        |> addMessage log.blockNumber
                            (Message.fromContractEvent
                                log.blockNumber
                                ssMessage
                            )
                        |> removeMiningMessage (Eth.Utils.txHashToString log.transactionHash)
                    , getBlockTimeIfNeededCmd prevModel.blockTimes log.blockNumber
                    )

        ShowOrHideAddress phaceId ->
            ( { prevModel
                | showAddress =
                    if prevModel.showAddress == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
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

        UnlockDai ->
            let
                ( newTxSentry, cmd ) =
                    let
                        txParams =
                            Dai.unlockDaiCall
                                |> Eth.toSend

                        listeners =
                            { onMined = Nothing
                            , onSign = Just UnlockMining
                            , onBroadcast = Nothing
                            }
                    in
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = newTxSentry
              }
            , cmd
            )

        UnlockMining broadcastResult ->
            case broadcastResult of
                Err errStr ->
                    ( prevModel
                        |> addUserNotice
                            (UN.web3BroadcastError "unlock DAI" errStr)
                    , Cmd.none
                    )

                Ok txHash ->
                    ( { prevModel
                        | composeUXModel =
                            prevModel.composeUXModel
                                |> updateMiningUnlockTx (Just txHash)
                      }
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
                        ( { prevModel
                            | wallet =
                                prevModel.wallet |> Wallet.withFetchedBalance balance
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
                                TokenValue.isMaxTokenValue allowance
                        in
                        ( { prevModel
                            | wallet =
                                prevModel.wallet |> Wallet.withIsUnlocked isUnlocked
                            , composeUXModel =
                                if isUnlocked then
                                    prevModel.composeUXModel
                                        |> updateMiningUnlockTx Nothing

                                else
                                    prevModel.composeUXModel
                          }
                        , Cmd.none
                        )

                    Err httpErr ->
                        ( prevModel
                            |> addUserNotice (UN.web3FetchError "DAI unlock status" httpErr)
                        , Cmd.none
                        )

        ShowComposeUX flag ->
            ( { prevModel
                | showComposeUX = flag
              }
            , Cmd.none
            )

        ReplyTo maybePostId ->
            let
                newShowComposeUX =
                    case maybePostId of
                        Just _ ->
                            True

                        _ ->
                            prevModel.showComposeUX
            in
            ( { prevModel
                | composeUXModel =
                    prevModel.composeUXModel
                        |> updateReply maybePostId
                , showComposeUX = newShowComposeUX
              }
            , Cmd.none
            )

        MessageInputChanged input ->
            ( { prevModel
                | composeUXModel =
                    prevModel.composeUXModel |> updateMessage input
              }
            , Cmd.none
            )

        DonationCheckboxSet flag ->
            ( { prevModel
                | composeUXModel =
                    prevModel.composeUXModel |> updateDonateChecked flag
              }
            , Cmd.none
            )

        DaiInputChanged input ->
            ( { prevModel
                | composeUXModel =
                    prevModel.composeUXModel |> updateDaiInput input
              }
            , Cmd.none
            )

        Submit messageDraft ->
            let
                ( newTxSentry, cmd ) =
                    let
                        txParams =
                            messageDraft
                                |> Message.encodeDraft
                                |> SSContract.burnEncodedMessage
                                |> Eth.toSend

                        listeners =
                            { onMined = Nothing
                            , onSign = Just <| SubmitSigned messageDraft
                            , onBroadcast = Nothing
                            }
                    in
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = newTxSentry
              }
            , cmd
            )

        SubmitSigned messageDraft txHashResult ->
            case txHashResult of
                Ok txHash ->
                    ( { prevModel
                        | composeUXModel =
                            prevModel.composeUXModel
                                |> updateMessage ""
                                |> updateReply Nothing
                        , miningMessages =
                            prevModel.miningMessages
                                |> Dict.insert (Eth.Utils.txHashToString txHash)
                                    (MiningMessage
                                        messageDraft
                                        Mining
                                    )
                      }
                    , Cmd.none
                    )

                Err errStr ->
                    ( prevModel
                        |> addUserNotice
                            (UN.web3SigError "post" errStr)
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

        DismissNotice id ->
            ( { prevModel
                | userNotices =
                    prevModel.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        NoOp ->
            ( prevModel, Cmd.none )

        ClickHappened ->
            ( { prevModel
                | showAddress = Nothing
              }
            , Cmd.none
            )


updateFromPageRoute : Route -> Model -> ( Model, Cmd Msg )
updateFromPageRoute route model =
    if model.route == route then
        ( model
        , Cmd.none
        )

    else
        gotoRoute route model


gotoRoute : Route -> Model -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    case route of
        Routing.InitialBlank ->
            ( prevModel, Cmd.none )

        Routing.Default ->
            ( { prevModel
                | viewFilter = None
                , route = route
              }
            , Cmd.none
            )

        Routing.ViewPost postIdInfoResult ->
            ( { prevModel
                | viewFilter = Post postIdInfoResult
                , route = route
              }
            , Cmd.none
            )

        Routing.NotFound ->
            ( { prevModel
                | route = route
              }
                |> addUserNotice UN.routeNotFound
            , Cmd.none
            )


addMessage : Int -> Message -> Model -> Model
addMessage blockNumber message prevModel =
    let
        alreadyHaveMessage =
            prevModel.messages
                |> Dict.get blockNumber
                |> Maybe.map
                    (List.any
                        (\listedMessage ->
                            listedMessage.postId == message.postId
                        )
                    )
                |> Maybe.withDefault False
    in
    if alreadyHaveMessage then
        prevModel

    else
        { prevModel
            | messages =
                prevModel.messages
                    |> Dict.update blockNumber
                        (\maybeMessagesForBlock ->
                            Just <|
                                case maybeMessagesForBlock of
                                    Nothing ->
                                        [ message ]

                                    Just messages ->
                                        List.append messages [ message ]
                        )
            , replies =
                List.append
                    prevModel.replies
                    (case message.metadata |> Result.toMaybe |> Maybe.andThen .replyTo of
                        Just replyTo ->
                            [ { from = message.postId
                              , to = replyTo
                              }
                            ]

                        Nothing ->
                            []
                    )
        }


removeMiningMessage : String -> Model -> Model
removeMiningMessage txHashString prevModel =
    { prevModel
        | miningMessages =
            prevModel.miningMessages
                |> Dict.remove txHashString
    }


getBlockTimeIfNeededCmd : Dict Int Time.Posix -> Int -> Cmd Msg
getBlockTimeIfNeededCmd blockTimes blockNumber =
    if Dict.get blockNumber blockTimes == Nothing then
        getBlockTimeCmd blockNumber

    else
        Cmd.none


fetchMessagesFromBlockrangeCmd : Eth.Types.BlockId -> Eth.Types.BlockId -> EventSentry Msg -> ( EventSentry Msg, Cmd Msg, EventSentry.Ref )
fetchMessagesFromBlockrangeCmd from to sentry =
    EventSentry.watch
        MessageLogReceived
        sentry
    <|
        SSContract.messageBurnEventFilter
            from
            to
            Nothing
            Nothing


fetchDaiBalanceAndAllowanceCmd : Address -> Cmd Msg
fetchDaiBalanceAndAllowanceCmd address =
    Cmd.batch
        [ Dai.getAllowanceCmd address (AllowanceFetched address)
        , Dai.getBalanceCmd address (BalanceFetched address)
        ]


getBlockTimeCmd : Int -> Cmd Msg
getBlockTimeCmd blocknum =
    Eth.getBlock
        Config.httpProviderUrl
        blocknum
        |> Task.map .timestamp
        |> Task.attempt (BlockTimeFetched blocknum)


addUserNotice : UserNotice Msg -> Model -> Model
addUserNotice notice model =
    model
        |> addUserNotices [ notice ]


addUserNotices : List (UserNotice Msg) -> Model -> Model
addUserNotices notices model =
    { model
        | userNotices =
            List.append
                model.userNotices
                notices
                |> List.Extra.uniqueBy .uniqueLabel
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , Time.every 2500 (always EveryFewSeconds)
        , Time.every 5000 (always CheckMiningMessagesStatus)
        , walletSentryPort
            (WalletSentry.decodeToMsg
                (WalletStatus << Err)
                (WalletStatus << Ok)
            )
        , TxSentry.listen model.txSentry
        ]


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port connectToWeb3 : () -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg
