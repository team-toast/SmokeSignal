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
import Json.Decode
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
            flags.chains
                |> List.filterMap
                    (Json.Decode.decodeValue Wallet.chainDecoder
                        >> Result.toMaybe
                    )
                |> List.foldl
                    (\data ->
                        case data.chain of
                            Types.XDai ->
                                \config_ ->
                                    { config_
                                        | xDai =
                                            { data | providerUrl = flags.xDaiProviderUrl }
                                    }

                            Types.Eth ->
                                \config_ ->
                                    { config_
                                        | ethereum =
                                            { data | providerUrl = flags.ethProviderUrl }
                                    }
                    )
                    model.config

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
                config.ethereum.providerUrl

        ( ethSentry, ethCmd1, ethCmd2 ) =
            startSentry config.ethereum

        ( xDaiSentry, xDaiCmd1, xDaiCmd2 ) =
            startSentry config.xDai

        now =
            Time.millisToPosix flags.nowInMillis
    in
    ( { model
        | view = view
        , wallet = wallet
        , now = now
        , dProfile = Helpers.Element.screenWidthToDisplayProfile flags.width
        , txSentry = txSentry
        , sentries =
            { xDai = xDaiSentry
            , ethereum = ethSentry
            }
        , userNotices = routingUserNotices
        , cookieConsentGranted = flags.cookieConsent
        , newUserModal = flags.newUser
        , config = config
        , alphaUrl = alphaUrl
      }
    , Cmd.batch
        [ ethCmd1
        , ethCmd2
        , xDaiCmd1
        , xDaiCmd2
        , Contracts.SmokeSignal.getEthPriceCmd
            config
            Types.EthPriceFetched
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
        , Browser.Events.onResize Types.Resize
        ]
