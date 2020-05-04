module Contracts.SmokeSignal exposing (..)

import Config
import Contracts.Generated.SmokeSignal as G
import Element
import Eth.Types exposing (..)
import Eth.Utils as U
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)
import Post
import TokenValue exposing (TokenValue)


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


burnEncodedPost : Post.EncodedDraft -> Call Hex
burnEncodedPost encodedPost =
    G.burnMessage
        Config.smokesignalContractAddress
        encodedPost.encodedMessageAndMetadata
        (TokenValue.getEvmValue encodedPost.burnAmount)
        (TokenValue.getEvmValue encodedPost.donateAmount)


fromMessageBurn : TxHash -> Int -> (String -> Element.Element Never) -> MessageBurn -> Post.Published
fromMessageBurn txHash block renderFunc messageEvent =
    let
        ( extractedMessage, extractedMetadata ) =
            case ( String.left 12 messageEvent.message, String.dropLeft 12 messageEvent.message ) of
                ( "!smokesignal", jsonStr ) ->
                    Post.decodeMessageAndMetadata jsonStr

                _ ->
                    ( messageEvent.message
                    , Post.nullMetadata
                    )
    in
    Post.Published
        txHash
        (Post.Id
            block
            messageEvent.hash
        )
        (Post.Core
            messageEvent.from
            messageEvent.burnAmount
            extractedMessage
            extractedMetadata
            (renderFunc extractedMessage)
        )
        Nothing
        Nothing
