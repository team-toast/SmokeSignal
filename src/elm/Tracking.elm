module Tracking exposing (composePostOpened, faucetRequestInitiated, metaMaskConnected, onboardingComplete, onboardingInitiated, postSubmitted, postTxMined, viewPost, xDaiClaimCompleted)

import Eth.Utils
import GTag exposing (gTagOut)
import Json.Encode as JE
import Ports
import Types exposing (PostId)


onboardingComplete : Cmd msg
onboardingComplete =
    { event = "onboarding-complete"
    , category = Nothing
    , label = Nothing
    , value = Nothing
    }
        |> gTagOut


viewPost : PostId -> Cmd msg
viewPost id =
    { name = "ViewContent"
    , data =
        [ ( "content_ids"
          , [ String.fromInt id.block
            , Eth.Utils.hexToString id.messageHash
            ]
                |> JE.list JE.string
          )
        ]
            |> JE.object
            |> Just
    , custom = False
    }
        |> Ports.fbEvent


composePostOpened : Cmd msg
composePostOpened =
    { name = "ComposePostOpened"
    , data = Nothing
    , custom = True
    }
        |> Ports.fbEvent


faucetRequestInitiated : Cmd msg
faucetRequestInitiated =
    { name = "FaucetRequestInitiated"
    , data = Nothing
    , custom = True
    }
        |> Ports.fbEvent


metaMaskConnected : Cmd msg
metaMaskConnected =
    { name = "MetaMaskConnected"
    , data = Nothing
    , custom = True
    }
        |> Ports.fbEvent


onboardingInitiated : Cmd msg
onboardingInitiated =
    Cmd.none


postTxMined : Cmd msg
postTxMined =
    { name = "PostTxMined"
    , data = Nothing
    , custom = True
    }
        |> Ports.fbEvent


xDaiClaimCompleted : Cmd msg
xDaiClaimCompleted =
    { name = "StartTrial"
    , data = Nothing
    , custom = False
    }
        |> Ports.fbEvent


postSubmitted : Cmd msg
postSubmitted =
    { name = "PostSubmitted"
    , data = Nothing
    , custom = True
    }
        |> Ports.fbEvent
