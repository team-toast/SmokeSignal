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


init : Wallet -> Model
init wallet =
    { message = ""
    , daiInput = ""
    , donateChecked = True
    , miningUnlockTx = Nothing
    , replyTo = Nothing
    , wallet = wallet
    }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        UpdateReplyTo maybePostId ->
            UpdateResult
                (prevModel |> updateReply maybePostId)
                Cmd.none
                (case maybePostId of
                    Just _ ->
                        [ ShowHalfComposeUX True ]

                    _ ->
                        []
                )

        MessageInputChanged input ->
            justModelUpdate
                (prevModel |> updateMessage input)

        DonationCheckboxSet flag ->
            justModelUpdate
                (prevModel |> updateDonateChecked flag)

        DaiInputChanged input ->
            justModelUpdate
                (prevModel |> updateDaiInput input)

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
                    |> updateReply Nothing
                )
                Cmd.none
                [ ShowHalfComposeUX False ]


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        []
