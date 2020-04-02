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
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Url exposing (Url)
import Wallet


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        -- until decided otherwise
        testMode =
            False

        wallet =
            if flags.networkId == 0 then
                let
                    _ =
                        maybeDebugLog "No web3 wallet detected" ""
                in
                Wallet.NoneDetected

            else
                Wallet.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId

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
                testMode
                initEventSentry
    in
    ( { wallet = wallet
      , now = Time.millisToPosix flags.nowInMillis
      , txSentry = txSentry
      , eventSentry = eventSentry
      , messages = []
      , showingAddress = Nothing
      , showComposeUX = False
      , composeUXModel = ComposeUXModel "" "" True
      , blockTimes = Dict.empty
      }
    , Cmd.batch
        [ initEventSentryCmd
        , secondEventSentryCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg prevModel =
    case msg of
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
                    let
                        _ =
                            maybeDebugLog "Error with WalletStatus Msg" errStr
                    in
                    ( prevModel, Cmd.none )

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
                    let
                        _ =
                            maybeDebugLog "Error decoding contract event" err
                    in
                    ( prevModel, Cmd.none )

                Ok ssMessage ->
                    ( { prevModel
                        | messages =
                            prevModel.messages
                                |> List.append
                                    [ Message
                                        ssMessage.hash
                                        log.blockNumber
                                        ssMessage.from
                                        ssMessage.burnAmount
                                        ssMessage.message
                                    ]
                      }
                    , if Dict.get log.blockNumber prevModel.blockTimes == Nothing then
                        getBlockTimeCmd log.blockNumber

                      else
                        Cmd.none
                    )

        ShowAddress address ->
            ( { prevModel | showingAddress = Just address }
            , Cmd.none
            )

        HideAddress ->
            ( { prevModel | showingAddress = Nothing }
            , Cmd.none
            )

        ConnectToWeb3 ->
            case prevModel.wallet of
                Wallet.NoneDetected ->
                    let
                        _ =
                            maybeDebugLog "no web3 detected" ""
                    in
                    ( prevModel
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
                            , onSign = Nothing
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
                        let
                            _ =
                                maybeDebugLog "http error at BalanceFetched" httpErr
                        in
                        ( prevModel, Cmd.none )

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
                        ( { prevModel
                            | wallet =
                                prevModel.wallet |> Wallet.withFetchedAllowance allowance
                          }
                        , Cmd.none
                        )

                    Err httpErr ->
                        let
                            _ =
                                maybeDebugLog "http error at AllowanceFetched" httpErr
                        in
                        ( prevModel, Cmd.none )

        ShowComposeUX flag ->
            ( { prevModel
                | showComposeUX = flag
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

        Submit validatedInputs ->
            let
                ( newTxSentry, cmd ) =
                    let
                        txParams =
                            SSContract.burnMessage
                                validatedInputs.message
                                validatedInputs.burnAmount
                                validatedInputs.donateAmount
                                |> Eth.toSend

                        listeners =
                            { onMined = Nothing
                            , onSign = Nothing
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

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err httpErr ->
                    let
                        _ =
                            maybeDebugLog "http error at BlockTimeFetched" httpErr
                    in
                    ( prevModel, Cmd.none )

                Ok time ->
                    ( { prevModel
                        | blockTimes =
                            prevModel.blockTimes
                                |> Dict.insert blocknum time
                      }
                    , Cmd.none
                    )

        NoOp ->
            ( prevModel, Cmd.none )

        ClickHappened ->
            ( prevModel, Cmd.none )

        Test s ->
            let
                _ =
                    maybeDebugLog "test" s
            in
            ( prevModel, Cmd.none )


fetchMessagesFromBlockrangeCmd : Eth.Types.BlockId -> Eth.Types.BlockId -> Bool -> EventSentry Msg -> ( EventSentry Msg, Cmd Msg, EventSentry.Ref )
fetchMessagesFromBlockrangeCmd from to testMode sentry =
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , Time.every 2500 (always EveryFewSeconds)
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
