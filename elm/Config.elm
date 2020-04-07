module Config exposing (..)

import BigInt exposing (BigInt)
import Eth.Types exposing (Address)
import Eth.Utils
import Time
import TokenValue exposing (TokenValue)


testMode =
    False


httpProviderUrl : String
httpProviderUrl =
    if testMode then
        ganacheHttpProviderUrl

    else
        mainnetHttpProviderUrl


mainnetHttpProviderUrl : String
mainnetHttpProviderUrl =
    "https://mainnet.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"


ganacheHttpProviderUrl : String
ganacheHttpProviderUrl =
    "http://localhost:8545"


daiContractAddress : Address
daiContractAddress =
    if testMode then
        Eth.Utils.unsafeToAddress "0xaD888d0Ade988EbEe74B8D4F39BF29a8d0fe8A8D"

    else
        Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"


smokesignalContractAddress : Address
smokesignalContractAddress =
    if testMode then
        Eth.Utils.unsafeToAddress "0x7c728214be9a0049e6a86f2137ec61030d0aa964"

    else
        Eth.Utils.unsafeToAddress "0xf18407C16ACF13a0057b2746B29F580eb8343156"


startScanBlock : Int
startScanBlock =
    if testMode then
        0

    else
        9786122


messageBurnEventSig =
    Eth.Utils.unsafeToHex "555288072588ecd9d95a72f03f4bf18f419c7edad2c7a56d3f0c56ec313857fe"
