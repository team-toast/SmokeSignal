module App exposing (main)

import Browser.Events
import Browser.Hashbang
import Browser.Navigation
import Chain
import Contracts.SmokeSignal
import Eth.Sentry.Event
import Eth.Sentry.Tx
import Eth.Types
import Helpers.Element
import Json.Decode
import Maybe.Extra exposing (unwrap)
import Misc exposing (tryRouteToView)
import Ports
import Routing
import Task
import Time
import Types exposing (Flags, Model, Msg)
import Update exposing (update)
import Url exposing (Url)
import UserNotice as UN
import View exposing (view)
import Wallet


main : Program Flags Model Msg
main =
    Browser.Hashbang.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Types.LinkClicked
        , onUrlChange = Routing.urlToRoute >> Types.RouteChanged
        }


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            Misc.emptyModel key
    in
    flags.chains
        |> Json.Decode.decodeValue
            (Chain.chainDecoder flags)
        |> Result.toMaybe
        |> unwrap
            ( { model
                | userNotices =
                    [ UN.unexpectedError "Config decode failure" ]
              }
            , Cmd.none
            )
            (\chainConfigs ->
                let
                    config =
                        chainConfigs
                            |> List.foldl
                                (\data ->
                                    case data.chain of
                                        Types.XDai ->
                                            \config_ ->
                                                { config_
                                                    | xDai = data
                                                }

                                        Types.Eth ->
                                            \config_ ->
                                                { config_
                                                    | ethereum = data
                                                }
                                )
                                model.config

                    redirectCmd =
                        Routing.blockParser url
                            |> Maybe.andThen
                                (\block ->
                                    if block < config.ethereum.startScanBlock then
                                        redirectDomain url

                                    else
                                        Nothing
                                )

                    modelWithConfig =
                        { model | config = config }
                in
                redirectCmd
                    |> unwrap
                        (startApp flags url modelWithConfig)
                        (\redirect ->
                            ( modelWithConfig
                            , redirect
                            )
                        )
            )


redirectDomain : Url -> Maybe (Cmd msg)
redirectDomain url =
    (case url.host of
        "smokesignal.eth.link" ->
            Just "alpha.smokesignal.eth.link"

        "smokesignal.eth" ->
            Just "alpha.smokesignal.eth"

        _ ->
            Nothing
    )
        |> Maybe.map
            (\newHost ->
                { url | host = newHost }
                    |> Url.toString
                    |> Browser.Navigation.load
            )


startApp : Flags -> Url -> Model -> ( Model, Cmd Msg )
startApp flags url model =
    let
        route =
            Routing.urlToRoute url

        alphaUrl =
            if String.endsWith ".eth" url.host then
                "http://alpha.smokesignal.eth"

            else
                "http://alpha.smokesignal.eth.link"

        ( view, routingUserNotices ) =
            case tryRouteToView route of
                Ok v ->
                    ( v, [] )

                Err err ->
                    ( Types.ViewHome
                    , [ UN.routeNotFound <| Just err ]
                    )

        wallet =
            if flags.hasWallet then
                Types.NetworkReady

            else
                Types.NoneDetected

        txSentry =
            Eth.Sentry.Tx.init
                ( Ports.txOut, Ports.txIn )
                (Types.TxSentryMsg Types.Eth)
                model.config.ethereum.providerUrl

        txSentryX =
            Eth.Sentry.Tx.init
                ( Ports.txOutX, Ports.txInX )
                (Types.TxSentryMsg Types.XDai)
                model.config.xDai.providerUrl

        ( ethSentry, ethCmd1, ethCmd2 ) =
            startSentry model.config.ethereum

        ( xDaiSentry, xDaiCmd1, xDaiCmd2 ) =
            startSentry model.config.xDai

        now =
            Time.millisToPosix flags.nowInMillis
    in
    ( { model
        | view = view
        , wallet = wallet
        , now = now
        , dProfile = Helpers.Element.screenWidthToDisplayProfile flags.width
        , txSentry = txSentry
        , txSentryX = txSentryX
        , sentries =
            model.sentries
                |> (\cs ->
                        { cs
                            | xDai = xDaiSentry
                            , ethereum = ethSentry
                        }
                   )
        , userNotices = routingUserNotices
        , cookieConsentGranted = flags.cookieConsent
        , newUserModal = flags.newUser
        , alphaUrl = alphaUrl
      }
    , Cmd.batch
        [ ethCmd1
        , ethCmd2
        , xDaiCmd1
        , xDaiCmd2
        , Contracts.SmokeSignal.getEthPriceCmd
            model.config.ethereum
            |> Task.attempt Types.EthPriceFetched
        , Ports.setDescription Misc.defaultSeoDescription
        ]
    )


startSentry : Types.ChainConfig -> ( Eth.Sentry.Event.EventSentry Msg, Cmd Msg, Cmd Msg )
startSentry config =
    let
        scan =
            Contracts.SmokeSignal.messageBurnEventFilter
                config.contract
                (Eth.Types.BlockNum config.startScanBlock)
                Eth.Types.LatestBlock
                Nothing
                Nothing

        ( initEventSentry, initEventSentryCmd ) =
            Eth.Sentry.Event.init (Types.EventSentryMsg config.chain)
                config.providerUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            Eth.Sentry.Event.watch
                (Contracts.SmokeSignal.decodePost config.chain
                    >> Types.PostLogReceived
                )
                initEventSentry
                scan
    in
    ( eventSentry, initEventSentryCmd, secondEventSentryCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Types.Tick
        , Time.every 2000 (always Types.ChangeDemoPhaceSrc)
        , Time.every 2500 (always Types.EveryFewSeconds)
        , Time.every 5000 (always Types.CheckTrackedTxsStatus)
        , Ports.walletResponse
            (Wallet.decodeConnectResponse >> Types.WalletResponse)
        , Eth.Sentry.Tx.listen model.txSentry
        , Eth.Sentry.Tx.listen model.txSentryX
        , Browser.Events.onResize Types.Resize
        ]
