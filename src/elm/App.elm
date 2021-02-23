module App exposing (main)

import Browser.Events
import Browser.Hashbang
import Browser.Navigation
import Contracts.SmokeSignal
import Eth.Sentry.Event
import Eth.Sentry.Tx
import Eth.Types
import Eth.Utils
import Helpers.Element
import Maybe.Extra
import Misc exposing (tryRouteToView)
import Ports
import Routing
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
        redirectCmd =
            Routing.blockParser url
                |> Maybe.andThen
                    (\block ->
                        if block < flags.startScanBlock then
                            redirectDomain url

                        else
                            Nothing
                    )

        model =
            Misc.emptyModel key
    in
    redirectCmd
        |> Maybe.Extra.unwrap
            (startApp flags url model)
            (\redirect ->
                ( model
                , redirect
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
        config =
            { smokeSignalContractAddress = Eth.Utils.unsafeToAddress flags.smokeSignalContractAddress
            , httpProviderUrl = flags.httpProviderUrl
            , startScanBlock = flags.startScanBlock
            }

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
                Types.TxSentryMsg
                config.httpProviderUrl

        ( initEventSentry, initEventSentryCmd ) =
            Eth.Sentry.Event.init Types.EventSentryMsg config.httpProviderUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            Contracts.SmokeSignal.messageBurnEventFilter
                config.smokeSignalContractAddress
                (Eth.Types.BlockNum config.startScanBlock)
                Eth.Types.LatestBlock
                Nothing
                Nothing
                |> Eth.Sentry.Event.watch
                    (Contracts.SmokeSignal.decodePost
                        >> Types.PostLogReceived
                    )
                    initEventSentry

        now =
            Time.millisToPosix flags.nowInMillis
    in
    ( { model
        | view = view
        , wallet = wallet
        , now = now
        , dProfile = Helpers.Element.screenWidthToDisplayProfile flags.width
        , txSentry = txSentry
        , eventSentry = eventSentry
        , userNotices = routingUserNotices
        , cookieConsentGranted = flags.cookieConsent
        , newUserModal = flags.newUser
        , config = config
        , alphaUrl = alphaUrl
      }
    , Cmd.batch
        [ initEventSentryCmd
        , secondEventSentryCmd
        , Contracts.SmokeSignal.getEthPriceCmd
            config
            Types.EthPriceFetched
        , Ports.setDescription Misc.defaultSeoDescription
        ]
    )


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
        , Browser.Events.onResize Types.Resize
        ]
