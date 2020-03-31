module Config exposing (..)

import BigInt exposing (BigInt)
import Eth.Types exposing (Address)
import Eth.Utils
import Time
import TokenValue exposing (TokenValue)


httpProviderUrl : String
httpProviderUrl =
    mainnetHttpProviderUrl


mainnetHttpProviderUrl : String
mainnetHttpProviderUrl =
    "https://mainnet.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"


daiContractAddress : Address
daiContractAddress =
    Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"


smokesignalContractAddress : Address
smokesignalContractAddress =
    Eth.Utils.unsafeToAddress "0x5DC963d2c78D8FAa90c4400eB0dE1e4aE763DBaE"


startScanBlock : Int
startScanBlock =
    9632692
