module Chain exposing (chainDecoder, decodeChain, getName)

import Eth.Decode
import Eth.Net
import Json.Decode as Decode exposing (Decoder)
import Types exposing (Chain(..), Flags)


getName : Chain -> String
getName chain =
    case chain of
        Eth ->
            "Eth"

        XDai ->
            "xDai"


chainDecoder : Flags -> Decoder (List Types.ChainConfig)
chainDecoder flags =
    Decode.map3
        (\chain contract scan ->
            { chain = chain
            , contract = contract
            , startScanBlock = scan
            , providerUrl =
                case chain of
                    Types.Eth ->
                        flags.ethProviderUrl

                    Types.XDai ->
                        flags.xDaiProviderUrl
            }
        )
        (Decode.field "network" decodeChain)
        (Decode.field "contract" Eth.Decode.address)
        (Decode.field "scan" Decode.int)
        |> Decode.list


decodeChain : Decoder Types.Chain
decodeChain =
    Eth.Net.networkIdDecoder
        |> Decode.andThen
            (\network ->
                case network of
                    Eth.Net.Mainnet ->
                        Types.Eth
                            |> Decode.succeed

                    Eth.Net.Private 100 ->
                        Types.XDai
                            |> Decode.succeed

                    -- Hardhat server
                    Eth.Net.Private 31337 ->
                        Types.Eth
                            |> Decode.succeed

                    _ ->
                        Decode.fail "bad network"
            )
