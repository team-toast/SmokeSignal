module App exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Chain
import Contracts.SmokeSignal
import DemoPhaceSrcMutator
import Dict
import Eth.Types
import Json.Decode
import Maybe.Extra exposing (unwrap)
import Misc exposing (emptyModel, tryRouteToView)
import Ports
import Random
import Routing
import Sentry
import Set
import Time
import TokenValue
import Types exposing (Chain(..), Flags, Model, Msg)
import Update exposing (update)
import Url exposing (Url)
import UserNotice as UN
import View exposing (view)
import Wallet


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    flags.chains
        |> Json.Decode.decodeValue
            (Chain.chainDecoder flags)
        |> Result.toMaybe
        |> unwrap
            ( { emptyModel
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
                                emptyModel.config

                    redirectCmd =
                        flags.href
                            |> Url.fromString
                            |> Maybe.andThen
                                (\url ->
                                    Routing.blockParser url
                                        |> Maybe.andThen
                                            (\block ->
                                                if block < config.ethereum.startScanBlock then
                                                    redirectDomain url

                                                else
                                                    Nothing
                                            )
                                )

                    modelWithConfig =
                        { emptyModel | config = config }
                in
                redirectCmd
                    |> unwrap
                        (startApp flags modelWithConfig)
                        (\redirect ->
                            ( modelWithConfig
                            , redirect
                            )
                        )
            )


alphaHost : String
alphaHost =
    "smokesignalalpha.eth"


redirectDomain : Url -> Maybe (Cmd msg)
redirectDomain url =
    (case url.host of
        "smokesignal.eth.link" ->
            Just <| alphaHost ++ ".link"

        "smokesignal.eth" ->
            Just alphaHost

        _ ->
            Nothing
    )
        |> Maybe.map
            (\newHost ->
                { url | host = newHost }
                    |> Url.toString
                    |> Browser.Navigation.load
            )


startApp : Flags -> Model -> ( Model, Cmd Msg )
startApp flags model =
    let
        posts =
            flags.posts
                |> List.filterMap
                    (Json.Decode.decodeValue
                        (Json.Decode.field "meta" Misc.decodeMeta
                            |> Json.Decode.andThen
                                (Contracts.SmokeSignal.decodeCachedPost
                                    >> Json.Decode.field "post"
                                )
                        )
                        >> Result.toMaybe
                    )

        latestEth =
            posts
                |> List.filterMap
                    (\p ->
                        case p of
                            Types.LogReply x ->
                                if x.core.chain == Eth then
                                    Just x.core.id.block

                                else
                                    Nothing

                            Types.LogRoot x ->
                                if x.core.chain == Eth then
                                    Just x.core.id.block

                                else
                                    Nothing
                    )
                |> List.sort
                |> List.reverse
                |> List.head

        latestXDai =
            posts
                |> List.filterMap
                    (\p ->
                        case p of
                            Types.LogReply x ->
                                if x.core.chain == XDai then
                                    Just x.core.id.block

                                else
                                    Nothing

                            Types.LogRoot x ->
                                if x.core.chain == XDai then
                                    Just x.core.id.block

                                else
                                    Nothing
                    )
                |> List.sort
                |> List.reverse
                |> List.head

        rootPosts =
            posts
                |> List.filterMap
                    (\p ->
                        case p of
                            Types.LogReply _ ->
                                Nothing

                            Types.LogRoot x ->
                                Just ( Misc.postIdToKey x.core.id, x )
                    )
                |> Dict.fromList

        topics =
            rootPosts
                |> Dict.values
                |> List.map
                    (\x ->
                        ( x.topic
                        , { total = TokenValue.zero
                          , ids = Set.empty
                          }
                        )
                    )
                |> Dict.fromList

        replies =
            posts
                |> List.filterMap
                    (\p ->
                        case p of
                            Types.LogReply x ->
                                Just ( Misc.postIdToKey x.core.id, x )

                            Types.LogRoot _ ->
                                Nothing
                    )
                |> Dict.fromList

        pages =
            rootPosts
                |> Update.calculatePagination
                    model.sortType
                    model.blockTimes
                    model.accounting
                    now

        replyIds =
            replies
                |> Dict.values
                |> List.foldl
                    (\post ->
                        Dict.update (Misc.postIdToKey post.parent)
                            (Maybe.withDefault Set.empty
                                >> Set.insert post.core.key
                                >> Just
                            )
                    )
                    Dict.empty

        route =
            Routing.parseRoute flags.href

        alphaUrl =
            flags.href
                |> Url.fromString
                |> unwrap
                    ("https://" ++ alphaHost)
                    (\url ->
                        if String.endsWith ".eth" url.host then
                            "https://" ++ alphaHost

                        else
                            "https://" ++ alphaHost ++ ".link"
                    )

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

        ( ethSentry, ethCmd ) =
            startSentry latestEth model.config.ethereum

        ( xDaiSentry, xDaiCmd ) =
            startSentry latestXDai model.config.xDai

        now =
            Time.millisToPosix flags.nowInMillis
    in
    ( { model
        | view = view
        , wallet = wallet
        , now = now
        , dProfile = Misc.screenWidthToDisplayProfile flags.width
        , sentries =
            model.sentries
                |> (\cs ->
                        { cs
                            | xDai = Just xDaiSentry
                            , ethereum = Just ethSentry
                        }
                   )
        , userNotices = routingUserNotices
        , cookieConsentGranted = flags.cookieConsent
        , newUserModal = flags.newUser
        , alphaUrl = alphaUrl
        , faucetToken = flags.faucetToken
        , shareEnabled = flags.shareEnabled
        , rootPosts = rootPosts
        , topics = topics
        , replyPosts = replies
        , replyIds = replyIds
        , pages = pages
      }
    , Cmd.batch
        [ ethCmd
        , xDaiCmd
        , Random.generate Types.NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
        , Ports.setDescription Misc.defaultSeoDescription
        ]
    )


startSentry : Maybe Int -> Types.ChainConfig -> ( Sentry.EventSentry Msg, Cmd Msg )
startSentry latest config =
    let
        scan =
            Contracts.SmokeSignal.messageBurnEventFilter
                config.contract
                (Eth.Types.BlockNum (latest |> Maybe.withDefault config.startScanBlock))
                Eth.Types.LatestBlock
                Nothing
                Nothing

        ( initEventSentry, initEventSentryCmd ) =
            Sentry.init (Types.EventSentryMsg config.chain)
                config.providerUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            Sentry.watch
                (Contracts.SmokeSignal.decodePost config.chain
                    >> Types.PostLogReceived
                )
                initEventSentry
                scan
    in
    ( eventSentry
    , Cmd.batch
        [ initEventSentryCmd
        , secondEventSentryCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Types.Tick
        , if model.wallet == Types.NoneDetected || model.wallet == Types.NetworkReady then
            Time.every 3000 (always Types.ChangeDemoPhaceSrc)

          else
            Sub.none
        , Time.every 5000 (always Types.CheckTrackedTxsStatus)
        , Ports.walletResponse
            (Wallet.walletInfoDecoder >> Types.WalletResponse)
        , Browser.Events.onResize Types.Resize
        , Ports.burnOrTipResponse (Wallet.rpcResponseDecoder >> Types.BurnOrTipResponse)
        , Ports.postResponse (Wallet.rpcResponseDecoder >> Types.PostResponse)
        , Ports.chainSwitchResponse (Wallet.chainSwitchDecoder >> Types.ChainSwitchResponse)
        , Ports.balanceResponse (Wallet.balanceDecoder >> Types.BalanceResponse)
        , Ports.onUrlChange (Routing.parseRoute >> Types.RouteChanged)
        ]
