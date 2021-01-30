module Contracts.SmokeSignal exposing (burnEncodedPost, burnForPost, decodePost, getAccountingCmd, getEthPriceCmd, messageBurnEventFilter, tipForPost)

import Contracts.Generated.SmokeSignal as G
import Eth
import Eth.Decode
import Eth.Types exposing (Address, BlockId, Call, Hex, LogFilter)
import Helpers.Eth as EthHelpers
import Http
import Json.Decode as Decode exposing (Decoder)
import Misc
import Post
import Result.Extra
import Task
import TokenValue exposing (TokenValue)
import Types exposing (..)


decodePost : Eth.Types.Log -> Eth.Types.Event (Result Decode.Error LogPost)
decodePost log =
    Eth.Decode.event
        (G.messageBurnDecoder
            |> Decode.andThen
                (\messageBurn ->
                    coreDecoder messageBurn
                        |> Decode.map
                            (\core ->
                                let
                                    id =
                                        { block = log.blockNumber
                                        , messageHash = messageBurn.hash
                                        }

                                    key =
                                        Misc.postIdToKey id

                                    core_ =
                                        { author = core.author
                                        , authorBurn = core.authorBurn
                                        , content = core.content
                                        , accounting = Nothing
                                        , metadataVersion = core.metadata.metadataVersion
                                        }
                                in
                                case core.metadata.context of
                                    TopLevel topic ->
                                        { id = id
                                        , key = key
                                        , txHash = log.transactionHash
                                        , core = core_
                                        , topic = topic
                                        }
                                            |> Types.LogRoot

                                    Reply parent ->
                                        { id = id
                                        , key = key
                                        , txHash = log.transactionHash
                                        , core = core_
                                        , parent = parent
                                        }
                                            |> Types.LogReply
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


coreDecoder : G.MessageBurn -> Decoder Core
coreDecoder messageBurn =
    case String.split "!smokesignal" messageBurn.message of
        [ _, encoded ] ->
            encoded
                |> Decode.decodeString
                    (Post.metadataDecoder
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
                    )
                |> Result.Extra.unpack
                    (Decode.errorToString >> Decode.fail)
                    Decode.succeed

        _ ->
            Decode.fail "Missing '!smokesignal'"
