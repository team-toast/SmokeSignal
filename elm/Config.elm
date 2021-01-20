module Config exposing (httpProviderUrl, initDemoPhaceSrc, smokesignalContractAddress, startScanBlock)

import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Types exposing (Id)


testMode : Bool
testMode =
    False


initDemoPhaceSrc : String
initDemoPhaceSrc =
    "2222222222222222222222222228083888c8f222"


httpProviderUrl : String
httpProviderUrl =
    if testMode then
        ganacheHttpProviderUrl

    else
        mainnetHttpProviderUrl


mainnetHttpProviderUrl : String
mainnetHttpProviderUrl =
    "https://d8a8054f0e9440f68e34f7ab44a78b09.eth.rpc.rivet.cloud/"


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
        Eth.Utils.unsafeToAddress "0x9f0Cec66a2893da5525D4339F83fD5B25A4E076B"


startScanBlock : Int
startScanBlock =
    if testMode then
        0

    else
        11679315
