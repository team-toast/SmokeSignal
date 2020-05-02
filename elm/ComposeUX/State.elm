module ComposeUX.State exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View
import ComposeUX.Types exposing (..)
import Contracts.Dai as Dai
import Contracts.SmokeSignal as SSContract
import Dict exposing (Dict)
import Element exposing (Element)
import Eth
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Utils
import Helpers.Time as TimeHelpers
import Post
import Time
import UserNotice as UN
import Wallet exposing (Wallet)


init : Time.Posix -> Wallet -> Post.Context -> Model
init now wallet context =
    { now = now
    , context = context
    , message = ""
    , daiInput = ""
    , donateChecked = True
    , miningUnlockTx = Nothing
    , wallet = wallet
    , showPreviewOnMobile = False
    , lastInputChangedTime = Time.millisToPosix 0
    , renderNeeded = False
    , renderedPreview = Nothing
    }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        MessageInputChanged input ->
            justModelUpdate
                ({ prevModel
                    | renderNeeded = True
                    , lastInputChangedTime = prevModel.now
                 }
                    |> updateMessage input
                )

        DonationCheckboxSet flag ->
            justModelUpdate
                (prevModel |> updateDonateChecked flag)

        DaiInputChanged input ->
            justModelUpdate
                (prevModel |> updateDaiInput input)

        MobilePreviewToggle ->
            justModelUpdate
                { prevModel
                    | showPreviewOnMobile =
                        not prevModel.showPreviewOnMobile
                }

        Tick time ->
            let
                msecSinceEdit =
                    TimeHelpers.sub
                        prevModel.now
                        prevModel.lastInputChangedTime
                        |> Time.posixToMillis
            in
            if prevModel.renderNeeded && (msecSinceEdit > 300) then
                justModelUpdate
                    { prevModel
                        | now = time
                        , renderedPreview =
                            renderPreviewIfNonEmpty prevModel.message
                        , renderNeeded = False
                    }

            else
                justModelUpdate
                    { prevModel
                        | now = time
                    }

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]


handleMsgDown : MsgDown -> Model -> UpdateResult
handleMsgDown msgDown prevModel =
    case msgDown of
        UpdateWallet newWallet ->
            justModelUpdate
                { prevModel | wallet = newWallet }


resetModel : Model -> Model
resetModel model =
    { model
        | message = ""
        , showPreviewOnMobile = False
        , daiInput = ""
        , renderedPreview = Nothing
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        []


renderPreviewIfNonEmpty : String -> Maybe (Element Never)
renderPreviewIfNonEmpty message =
    if message == "" then
        Nothing

    else
        Just <| Common.View.renderContentOrError message
