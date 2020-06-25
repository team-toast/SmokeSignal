module Config exposing (..)

import BigInt exposing (BigInt)
import Eth.Types exposing (Address)
import Eth.Utils
import Post
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
    "https://2902ba438aac4b0294ead60c7a2e0aa3.eth.rpc.rivet.cloud/"


ganacheHttpProviderUrl : String
ganacheHttpProviderUrl =
    "http://localhost:8545"


daiContractAddress : Address
daiContractAddress =
    if testMode then
        Eth.Utils.unsafeToAddress "0xCfEB869F69431e42cdB54A4F4f105C19C080A601"

    else
        Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"


smokesignalContractAddress : Address
smokesignalContractAddress =
    if testMode then
        Eth.Utils.unsafeToAddress "0x254dffcd3277C0b1660F6d42EFbB754edaBAbC2B"

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


moreInfoPostId : Post.Id
moreInfoPostId =
    { block = 9956175
    , messageHash = Eth.Utils.unsafeToHex "0x005217c20d13cb47dee70a55b7f705044ac67789036133ff6944340b1f937d2a"
    }
