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
import Task exposing (Task)
import TokenValue exposing (TokenValue)
import Types exposing (..)


decodePost : Chain -> Eth.Types.Log -> Eth.Types.Event (Result Decode.Error LogPost)
decodePost chain log =
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
                                        { id = id
                                        , key = key
                                        , txHash = log.transactionHash
                                        , author = core.author
                                        , authorBurn = core.authorBurn
                                        , content = core.content
                                        , metadataVersion = core.metadata.metadataVersion
                                        , chain = chain
                                        }
                                in
                                case core.metadata.context of
                                    TopLevel topic ->
                                        { core = core_
                                        , topic = topic
                                        }
                                            |> Types.LogRoot

                                    Reply parent ->
                                        { core = core_
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


burnEncodedPost : UserInfo -> Address -> EncodedDraft -> Call Hex
burnEncodedPost wallet smokeSignalContractAddress encodedPost =
    G.burnMessage
        smokeSignalContractAddress
        encodedPost.encodedContentAndMetadata
        (TokenValue.getEvmValue encodedPost.donateAmount)
        |> EthHelpers.updateCallValue (TokenValue.getEvmValue encodedPost.burnAmount)
        |> (\call ->
                { call | from = Just wallet.address }
           )


getAccountingCmd : ChainConfig -> Hex -> Task Http.Error Accounting
getAccountingCmd config msgHash =
    Eth.call
        config.providerUrl
        (G.storedMessageData
            config.contract
            msgHash
        )
        |> Task.map
            (\storedMessageData ->
                { firstAuthor = storedMessageData.firstAuthor
                , totalBurned = TokenValue.tokenValue storedMessageData.dollarsBurned
                , totalTipped = TokenValue.tokenValue storedMessageData.dollarsTipped
                }
            )


getEthPriceCmd : ChainConfig -> Task Http.Error Float
getEthPriceCmd config =
    Eth.call
        config.providerUrl
        (G.ethPrice config.contract)
        |> Task.map
            (TokenValue.tokenValue
                >> TokenValue.toFloatWithWarning
            )


tipForPost : UserInfo -> Address -> Hex -> TokenValue -> Bool -> Call ()
tipForPost wallet smokeSignalContractAddress messageHash amount donate =
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
        |> (\call ->
                { call | from = Just wallet.address }
           )


burnForPost : UserInfo -> Address -> Hex -> TokenValue -> Bool -> Call ()
burnForPost wallet smokeSignalContractAddress messageHash amount donate =
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
        |> (\call ->
                { call | from = Just wallet.address }
           )


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
