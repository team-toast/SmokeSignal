module Tracking exposing (composePostOpened, faucetRequestInitiated, metaMaskConnected, onboardingComplete, onboardingInitiated, postSubmitted, postTxMined, viewPost, xDaiClaimCompleted)

import Eth.Utils
import GTag exposing (gTagOut)
import Json.Encode as JE
import Ports
import Types exposing (PostId)


getTag : String -> String
getTag name =
    let
        isStandardEvent =
            -- Standard Events:
            -- https://developers.facebook.com/docs/facebook-pixel/reference
            [ "ViewContent"
            , "StartTrial"
            ]
                |> List.member name
    in
    if isStandardEvent then
        "track"

    else
        "trackCustom"


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
    let
        name =
            "ViewContent"
    in
    { name = name
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
    , tag = getTag name
    }
        |> Ports.fbEvent


composePostOpened : Cmd msg
composePostOpened =
    let
        name =
            "ComposePostOpened"
    in
    [ { name = name
      , data = Nothing
      , tag = getTag name
      }
        |> Ports.fbEvent
    , { event = name
      , category = Nothing
      , label = Nothing
      , value = Nothing
      }
        |> gTagOut
    ]
        |> Cmd.batch


faucetRequestInitiated : Cmd msg
faucetRequestInitiated =
    let
        name =
            "FaucetRequestInitiated"
    in
    [ { name = name
      , data = Nothing
      , tag = getTag name
      }
        |> Ports.fbEvent
    , { event = name
      , category = Nothing
      , label = Nothing
      , value = Nothing
      }
        |> gTagOut
    ]
        |> Cmd.batch


metaMaskConnected : Cmd msg
metaMaskConnected =
    let
        name =
            "MetaMaskConnected"
    in
    { name = name
    , data = Nothing
    , tag = getTag name
    }
        |> Ports.fbEvent


onboardingInitiated : Cmd msg
onboardingInitiated =
    Cmd.none


postTxMined : Cmd msg
postTxMined =
    let
        name =
            "PostTxMined"
    in
    { name = name
    , data = Nothing
    , tag = getTag name
    }
        |> Ports.fbEvent


xDaiClaimCompleted : Cmd msg
xDaiClaimCompleted =
    let
        name =
            "StartTrial"
    in
    [ { name = name
      , data = Nothing
      , tag = getTag name
      }
        |> Ports.fbEvent
    , { event = name
      , category = Nothing
      , label = Nothing
      , value = Nothing
      }
        |> gTagOut
    ]
        |> Cmd.batch


postSubmitted : Cmd msg
postSubmitted =
    let
        name =
            "PostSubmitted"
    in
    { name = name
    , data = Nothing
    , tag = getTag name
    }
        |> Ports.fbEvent
