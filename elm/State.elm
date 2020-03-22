port module State exposing (init, subscriptions, update)

import Browser
import Browser.Events
import Browser.Navigation
import CommonTypes exposing (..)
import Config
import Contracts.Dai as Dai
import Contracts.SmokeSig as SSContract
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
import Time
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
                        Debug.log "No web3 wallet detected" ""
                in
                Wallet.NoneDetected

            else
                Wallet.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId

        txSentry =
            TxSentry.init
                ( txOut, txIn )
                TxSentryMsg
                (Config.httpProviderUrl testMode)

        ( initEventSentry, initEventSentryCmd ) =
            EventSentry.init EventSentryMsg (Config.httpProviderUrl testMode)

        ( eventSentry, secondEventSentryCmd, _ ) =
            fetchMessagesFromBlockrangeCmd
                (Eth.Types.BlockNum Config.startScanBlock)
                Eth.Types.LatestBlock
                testMode
                initEventSentry
    in
    ( { wallet = wallet
      , now = Time.millisToPosix flags.nowInMillis
      , testMode = testMode
      , txSentry = txSentry
      , eventSentry = eventSentry
      , userBalance = Nothing
      , userAllowance = Nothing
      , messages = []
      , showMessageInput = False
      , composeUXModel = ComposeUXModel "" ""
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
                        fetchDaiBalanceAndAllowanceCmd userInfo.address prevModel.testMode
                    )
                |> Maybe.withDefault Cmd.none
            )

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        newWallet =
                            case walletSentry.account of
                                Just address ->
                                    Wallet.Active <|
                                        UserInfo
                                            walletSentry.networkId
                                            address

                                Nothing ->
                                    Wallet.OnlyNetwork walletSentry.networkId
                    in
                    ( { prevModel
                        | wallet = newWallet
                      }
                    , Cmd.none
                    )

                Err errStr ->
                    let
                        _ =
                            Debug.log "Error with WalletStatus Msg" errStr
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
                    Eth.Decode.event SSContract.smokeSignalWithMessageDecoder log
            in
            case decodedEventLog.returnData of
                Err err ->
                    let
                        _ =
                            Debug.log "Error decoding contract event" err
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
                    , Cmd.none
                    )

        ConnectToWeb3 ->
            case prevModel.wallet of
                Wallet.NoneDetected ->
                    let
                        _ =
                            Debug.log "no web3 detected" ""
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
                                prevModel.testMode
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

        BalanceFetched fetchResult ->
            case fetchResult of
                Ok balance ->
                    ( { prevModel | userBalance = Just balance }
                    , Cmd.none
                    )

                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error at BalanceFetched" httpErr
                    in
                    ( prevModel, Cmd.none )

        AllowanceFetched fetchResult ->
            case fetchResult of
                Ok allowance ->
                    ( { prevModel | userAllowance = Just allowance }
                    , Cmd.none
                    )

                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error at AllowanceFetched" httpErr
                    in
                    ( prevModel, Cmd.none )

        ComposeMessage ->
            ( { prevModel
                | showMessageInput = True
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

        DaiInputChanged input ->
            ( { prevModel
                | composeUXModel =
                    prevModel.composeUXModel |> updateDaiInput input
              }
            , Cmd.none
            )

        Submit validatedInputs ->
            Debug.todo ""

        NoOp ->
            ( prevModel, Cmd.none )

        ClickHappened ->
            ( prevModel, Cmd.none )

        Test s ->
            let
                _ =
                    Debug.log "test" s
            in
            ( prevModel, Cmd.none )


fetchMessagesFromBlockrangeCmd : Eth.Types.BlockId -> Eth.Types.BlockId -> Bool -> EventSentry Msg -> ( EventSentry Msg, Cmd Msg, EventSentry.Ref )
fetchMessagesFromBlockrangeCmd from to testMode sentry =
    EventSentry.watch
        MessageLogReceived
        sentry
    <|
        SSContract.smokeSignalWithMessageEventFilter
            testMode
            from
            to


fetchDaiBalanceAndAllowanceCmd : Address -> Bool -> Cmd Msg
fetchDaiBalanceAndAllowanceCmd address testMode =
    Cmd.batch
        [ Dai.getAllowanceCmd address testMode AllowanceFetched
        , Dai.getBalanceCmd address testMode BalanceFetched
        ]


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
