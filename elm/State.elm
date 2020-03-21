port module State exposing (init, subscriptions, update)

import Browser
import Browser.Events
import Browser.Navigation
import CommonTypes exposing (..)
import Config
import Contracts.SmokeSig as SSContract
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
            Wallet.httpProvider wallet
                |> Maybe.map
                    (\httpProvider ->
                        TxSentry.init ( txOut, txIn ) TxSentryMsg httpProvider
                    )

        ( initEventSentry, initEventSentryCmd ) =
            EventSentry.init EventSentryMsg (Config.httpProviderUrl testMode)

        ( eventSentry, secondEventSentryCmd, _ ) =
            fetchMessagesFromBlockrangeCmd
                (Eth.Types.BlockNum 9632692)
                Eth.Types.LatestBlock
                testMode
                initEventSentry
    in
    ( { wallet = wallet
      , now = Time.millisToPosix flags.nowInMillis
      , testMode = testMode
      , txSentry = txSentry
      , eventSentry = eventSentry
      , messages = []
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
                    case prevModel.txSentry of
                        Just txSentry ->
                            TxSentry.update subMsg txSentry
                                |> Tuple.mapFirst Just

                        Nothing ->
                            ( Nothing, Cmd.none )
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

        ClickHappened ->
            ( prevModel, Cmd.none )

        NoOp ->
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , walletSentryPort
            (WalletSentry.decodeToMsg
                (WalletStatus << Err)
                (WalletStatus << Ok)
            )
        , Maybe.map TxSentry.listen model.txSentry
            |> Maybe.withDefault Sub.none
        ]


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port connectToWeb3 : () -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg
