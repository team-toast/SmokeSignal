module Contracts.SmokeSignal exposing (..)

import Helpers.Eth as EthHelpers
import Config
import Contracts.Generated.SmokeSignal as G
import Element
import Eth
import Eth.Types exposing (..)
import Eth.Utils as U
import Http
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)
import Post
import Task
import TokenValue exposing (TokenValue)
import Types exposing (Accounting, Content, Core, EncodedDraft, Id, Published)


type alias MessageBurn =
    { hash : Hex
    , from : Address
    , burnAmount : TokenValue
    , message : String
    }


convertBurnAmount : G.MessageBurn -> MessageBurn
convertBurnAmount gen =
    MessageBurn
        gen.hash
        gen.from
        (TokenValue.tokenValue gen.burnAmount)
        gen.message


messageBurnEventFilter : BlockId -> BlockId -> Maybe Hex -> Maybe Address -> LogFilter
messageBurnEventFilter from to maybeHash maybeAuthor =
    G.messageBurnEvent
        Config.smokesignalContractAddress
        maybeHash
        maybeAuthor
        |> (\filter ->
                { filter
                    | fromBlock = from
                    , toBlock = to
                }
           )


messageBurnDecoder : Decoder MessageBurn
messageBurnDecoder =
    G.messageBurnDecoder
        |> Decode.map convertBurnAmount


burnEncodedPost : EncodedDraft -> Call Hex
burnEncodedPost encodedPost =
    G.burnMessage
        Config.smokesignalContractAddress
        encodedPost.encodedContentAndMetadata
        (TokenValue.getEvmValue encodedPost.donateAmount)
        |> EthHelpers.updateCallValue (TokenValue.getEvmValue encodedPost.burnAmount)


fromMessageBurn : TxHash -> Int -> (Content -> Element.Element msg) -> MessageBurn -> Published
fromMessageBurn txHash block renderFunc messageEvent =
    let
        ( extractedMetadata, extractedMessage ) =
            case ( String.left 12 messageEvent.message, String.dropLeft 12 messageEvent.message ) of
                ( "!smokesignal", jsonStr ) ->
                    Post.decodePostData jsonStr

                _ ->
                    ( Post.nullMetadata
                    , Post.justBodyContent messageEvent.message
                    )
    in
    Published
        txHash
        (Id
            block
            messageEvent.hash
        )
        (Core
            messageEvent.from
            messageEvent.burnAmount
            extractedMessage
            extractedMetadata
         --(renderFunc extractedMessage)
        )
        Nothing


toAccounting : G.StoredMessageData -> Accounting
toAccounting storedMessageData =
    Accounting
        storedMessageData.firstAuthor
        (TokenValue.tokenValue storedMessageData.nativeBurned)
        (TokenValue.tokenValue storedMessageData.nativeTipped)


getAccountingCmd : Hex -> (Result Http.Error Accounting -> msg) -> Cmd msg
getAccountingCmd msgHash msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (G.storedMessageData
            Config.smokesignalContractAddress
            msgHash
        )
        |> Task.map toAccounting
        |> Task.attempt msgConstructor


tipForPost : Hex -> TokenValue -> Bool -> Call ()
tipForPost messageHash amount donate =
    G.tipHashOrBurnIfNoAuthor
        Config.smokesignalContractAddress
        messageHash
        (if donate then
            TokenValue.div
                amount
                100
                |> TokenValue.getEvmValue

         else
            TokenValue.zero |> TokenValue.getEvmValue
        )
        |> EthHelpers.updateCallValue (TokenValue.getEvmValue amount)


burnForPost : Hex -> TokenValue -> Bool -> Call ()
burnForPost messageHash amount donate =
    G.burnHash
        Config.smokesignalContractAddress
        messageHash
        (if donate then
            TokenValue.div
                amount
                100
                |> TokenValue.getEvmValue

         else
            TokenValue.zero |> TokenValue.getEvmValue
        )
        |> EthHelpers.updateCallValue (TokenValue.getEvmValue amount)
