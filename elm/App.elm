module App exposing (main)

import Browser.Events
import Browser.Hash
import Browser.Navigation
import Config
import Contracts.SmokeSignal
import Eth.Net
import Eth.Sentry.Event exposing (EventSentry)
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Eth.Types
import Helpers.Element
import Misc
import Ports
import Routing
import Time
import Types exposing (..)
import Update exposing (update)
import Url exposing (Url)
import UserNotice
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.Hash.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Routing.urlToRoute url

        ( wallet, walletNotices ) =
            if flags.networkId == 0 then
                ( Types.NoneDetected
                , [ UserNotice.noWeb3Provider ]
                )

            else
                ( Types.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId
                , []
                )

        txSentry =
            Eth.Sentry.Tx.init
                ( Ports.txOut, Ports.txIn )
                TxSentryMsg
                Config.httpProviderUrl

        ( initEventSentry, initEventSentryCmd ) =
            Eth.Sentry.Event.init EventSentryMsg Config.httpProviderUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            Contracts.SmokeSignal.messageBurnEventFilter
                (Eth.Types.BlockNum Config.startScanBlock)
                Eth.Types.LatestBlock
                Nothing
                Nothing
                |> Eth.Sentry.Event.watch
                    PostLogReceived
                    initEventSentry

        --fetchPostsFromBlockrangeCmd
        --(Eth.Types.BlockNum Config.startScanBlock)
        --Eth.Types.LatestBlock
        --initEventSentry
        now =
            Time.millisToPosix flags.nowInMillis

        model =
            Misc.emptyModel key
    in
    ( { model
        | basePath = flags.basePath
        , route = route
        , wallet = wallet
        , now = now
        , mode = Types.ModeHome
        , dProfile = Helpers.Element.screenWidthToDisplayProfile flags.width
        , txSentry = txSentry
        , eventSentry = eventSentry
        , userNotices = walletNotices
        , demoPhaceSrc = Config.initDemoPhaceSrc
        , cookieConsentGranted = flags.cookieConsent
      }
    , Cmd.batch
        [ initEventSentryCmd
        , secondEventSentryCmd

        --, routeCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , Time.every 2000 (always ChangeDemoPhaceSrc)
        , Time.every 2500 (always EveryFewSeconds)
        , Time.every 5000 (always CheckTrackedTxsStatus)
        , Ports.walletSentryPort
            (Eth.Sentry.Wallet.decodeToMsg
                (WalletStatus << Err)
                (WalletStatus << Ok)
            )
        , Eth.Sentry.Tx.listen model.txSentry
        , Browser.Events.onResize Resize

        --, Sub.map ComposeUXMsg ComposeUX.subscriptions
        ]
