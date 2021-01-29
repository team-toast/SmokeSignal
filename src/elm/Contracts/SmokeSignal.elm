module Contracts.SmokeSignal exposing (burnEncodedPost, burnForPost, decodePost, getAccountingCmd, getEthPriceCmd, messageBurnEventFilter, tipForPost)

import Contracts.Generated.SmokeSignal as G
import Eth
import Eth.Decode
import Eth.Types exposing (Address, BlockId, Call, Hex, LogFilter)
import Eth.Utils
import Helpers.Eth as EthHelpers
import Http
import Json.Decode as Decode exposing (Decoder)
import Post exposing (nullMetadata)
import Result.Extra
import Task
import TokenValue exposing (TokenValue)
import Types exposing (..)


decodePost : Eth.Types.Log -> Eth.Types.Event (Result Decode.Error Published)
decodePost log =
    Eth.Decode.event
        (G.messageBurnDecoder
            |> Decode.andThen
                (\messageBurn ->
                    parseCore messageBurn
                        |> Decode.map
                            (\core ->
                                { txHash = log.transactionHash
                                , key =
                                    ( String.fromInt log.blockNumber
                                    , Eth.Utils.hexToString messageBurn.hash
                                    )
                                , id =
                                    { block = log.blockNumber
                                    , messageHash = messageBurn.hash
                                    }
                                , core = core
                                , maybeAccounting = Nothing
                                }
                            )
                )
        )
        log


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


burnEncodedPost : Address -> EncodedDraft -> Call Hex
burnEncodedPost smokeSignalContractAddress encodedPost =
    G.burnMessage
        smokeSignalContractAddress
        encodedPost.encodedContentAndMetadata
        (TokenValue.getEvmValue encodedPost.donateAmount)
        |> EthHelpers.updateCallValue (TokenValue.getEvmValue encodedPost.burnAmount)


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


parseCore : G.MessageBurn -> Decoder Core
parseCore messageBurn =
    let
        encoded =
            String.dropLeft 12 messageBurn.message
    in
    case String.left 12 messageBurn.message of
        "!smokesignal" ->
            encoded
                |> Decode.decodeString (coreDecoder messageBurn)
                |> Result.Extra.unpack (Decode.errorToString >> Decode.fail) Decode.succeed

        _ ->
            Decode.fail "Missing '!smokesignal'"


coreDecoder : G.MessageBurn -> Decoder Core
coreDecoder messageBurn =
    Post.metadataDecoder
        |> Decode.andThen
            (\metadata ->
                Post.messageDataDecoder metadata.metadataVersion
                    |> Decode.map
                        (\content ->
                            { author = messageBurn.from
                            , authorBurn = TokenValue.tokenValue messageBurn.burnAmount
                            , content = content
                            , metadata = metadata
                            }
                        )
            )
