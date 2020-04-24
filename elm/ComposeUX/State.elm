module ComposeUX.State exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import ComposeUX.Types exposing (..)
import Contracts.Dai as Dai
import Contracts.SmokeSignal as SSContract
import Dict exposing (Dict)
import Eth
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Utils
import Post
import UserNotice as UN
import Wallet exposing (Wallet)


init : Wallet -> Post.Context -> Model
init wallet context =
    { context = context
    , message = ""
    , daiInput = ""
    , donateChecked = True
    , miningUnlockTx = Nothing
    , wallet = wallet
    , showPreviewOnMobile = False
    }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        MessageInputChanged input ->
            justModelUpdate
                (prevModel |> updateMessage input)

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

        PostSigned messageDraft ->
            UpdateResult
                (prevModel
                    |> updateMessage ""
                )
                Cmd.none
                [ HideHalfCompose ]


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        []
