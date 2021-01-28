module Contracts.SmokeSignal exposing (burnEncodedPost, burnForPost, fromMessageBurn, getAccountingCmd, getEthPriceCmd, messageBurnDecoder, messageBurnEventFilter, tipForPost)

import Contracts.Generated.SmokeSignal as G
import Eth
import Eth.Types exposing (Address, BlockId, Call, Hex, LogFilter, TxHash)
import Helpers.Eth as EthHelpers
import Http
import Json.Decode as Decode exposing (Decoder)
import Post exposing (nullMetadata)
import Result.Extra
import Task
import TokenValue exposing (TokenValue)
import Types exposing (..)


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


messageBurnEventFilter : Address -> BlockId -> BlockId -> Maybe Hex -> Maybe Address -> LogFilter
messageBurnEventFilter smokeSignalContractAddress from to maybeHash maybeAuthor =
    G.messageBurnEvent
        smokeSignalContractAddress
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


burnEncodedPost : Address -> EncodedDraft -> Call Hex
burnEncodedPost smokeSignalContractAddress encodedPost =
    G.burnMessage
        smokeSignalContractAddress
        encodedPost.encodedContentAndMetadata
        (TokenValue.getEvmValue encodedPost.donateAmount)
        |> EthHelpers.updateCallValue (TokenValue.getEvmValue encodedPost.burnAmount)


fromMessageBurn : TxHash -> Int -> MessageBurn -> Published
fromMessageBurn txHash block messageEvent =
    { txHash = txHash
    , id =
        { block = block
        , messageHash = messageEvent.hash
        }
    , core = parseCore messageEvent
    , maybeAccounting = Nothing
    }


toAccounting : G.StoredMessageData -> Accounting
toAccounting storedMessageData =
    Accounting
        storedMessageData.firstAuthor
        (TokenValue.tokenValue storedMessageData.nativeBurned)
        (TokenValue.tokenValue storedMessageData.nativeTipped)


getAccountingCmd : Config -> Hex -> (Result Http.Error Accounting -> msg) -> Cmd msg
getAccountingCmd config msgHash msgConstructor =
    Eth.call
        config.httpProviderUrl
        (G.storedMessageData
            config.smokeSignalContractAddress
            msgHash
        )
        |> Task.map toAccounting
        |> Task.attempt msgConstructor


getEthPriceCmd : Config -> (Result Http.Error Float -> msg) -> Cmd msg
getEthPriceCmd config msgConstructor =
    Eth.call
        config.httpProviderUrl
        (G.ethPrice config.smokeSignalContractAddress)
        |> Task.map TokenValue.tokenValue
        |> Task.map TokenValue.toFloatWithWarning
        |> Task.attempt msgConstructor


tipForPost : Address -> Hex -> TokenValue -> Bool -> Call ()
tipForPost smokeSignalContractAddress messageHash amount donate =
    G.tipHashOrBurnIfNoAuthor
        smokeSignalContractAddress
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


burnForPost : Address -> Hex -> TokenValue -> Bool -> Call ()
burnForPost smokeSignalContractAddress messageHash amount donate =
    G.burnHash
        smokeSignalContractAddress
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


parseCore : MessageBurn -> Core
parseCore messageEvent =
    let
        encoded =
            String.dropLeft 12 messageEvent.message
    in
    case String.left 12 messageEvent.message of
        "!smokesignal" ->
            encoded
                |> Decode.decodeString (coreDecoder messageEvent)
                |> Result.Extra.extract
                    (\decodeErr ->
                        { author = messageEvent.from
                        , authorBurn = messageEvent.burnAmount
                        , content = Post.justBodyContent encoded
                        , metadata =
                            { nullMetadata
                                | maybeDecodeError =
                                    Decode.errorToString decodeErr
                                        |> Just
                            }
                        }
                    )

        _ ->
            { author = messageEvent.from
            , authorBurn = messageEvent.burnAmount
            , content = Post.justBodyContent encoded
            , metadata = nullMetadata
            }


coreDecoder : MessageBurn -> Decoder Core
coreDecoder messageBurn =
    Post.metadataDecoder
        |> Decode.andThen
            (\metadata ->
                Post.messageDataDecoder metadata.metadataVersion
                    |> Decode.map
                        (\content ->
                            { author = messageBurn.from
                            , authorBurn = messageBurn.burnAmount
                            , content = content
                            , metadata = metadata
                            }
                        )
            )
