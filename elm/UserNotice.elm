module UserNotice exposing (..)

import Element exposing (Attribute, Element)
import Element.Font
import Http
import Json.Decode
import MaybeDebugLog exposing (maybeDebugLog)


type alias UserNotice msg =
    { noticeType : NoticeType
    , mainParagraphs : List (List (Element msg))
    , align : Alignment
    , uniqueLabel : String
    }


type Alignment
    = BottomRight
    | TopLeft


map : (msg1 -> msg2) -> UserNotice msg1 -> UserNotice msg2
map f userNotice =
    UserNotice
        userNotice.noticeType
        (userNotice.mainParagraphs
            |> List.map (List.map (Element.map f))
        )
        userNotice.align
        userNotice.uniqueLabel


type NoticeType
    = Update
    | Caution
    | Error
    | ShouldBeImpossible


placeholderNotice s =
    UserNotice
        Caution
        [ [ Element.text s ] ]


screenToSmall : Int -> UserNotice msg
screenToSmall width =
    { uniqueLabel = "screenToSmall " ++ (String.fromInt width)
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "Your screen is quite small (" ++ String.fromInt width ++ " across)--things may be broken!." ] ]
    , align = TopLeft
    }


noWeb3Provider : UserNotice msg
noWeb3Provider =
    { uniqueLabel = "noWeb3Provider"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "No web3 provider detected. Is "
          , Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                { url = "https://metamask.io/"
                , label = Element.text "Metamask"
                }
          , Element.text " or some other web3 provider installed and unlocked?"
          ]
        ]
    , align = BottomRight
    }


noWeb3Account : UserNotice msg
noWeb3Account =
    { uniqueLabel = "noWeb3Account"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "I can't detect a web3 account. Your wallet may be locked." ]
        ]
    , align = BottomRight
    }


cantConnectNoWeb3 : UserNotice msg
cantConnectNoWeb3 =
    { uniqueLabel = "cantConnectNoWeb3"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "You need a web3 provider (such as "
          , Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                { url = "https://metamask.io/"
                , label = Element.text "Metamask"
                }
          , Element.text ") to Connect."
          ]
        , [ Element.text "Until you connect, SmokeSignal will operate in read-only mode." ]
        ]
    , align = BottomRight
    }


wrongWeb3Network : UserNotice msg
wrongWeb3Network =
    { uniqueLabel = "wrongWeb3Network"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text "SmokeSignal only works on Ethereum mainnet. Make sure your wallet is set to Ethereum mainnet." ]
        ]
    , align = BottomRight
    }


unexpectedError : String -> a -> UserNotice msg
unexpectedError text debugObj =
    let
        _ =
            maybeDebugLog text debugObj
    in
    { uniqueLabel = "unexpectedError " ++ text
    , noticeType = ShouldBeImpossible
    , mainParagraphs = [ [ Element.text text ] ]
    , align = BottomRight
    }


eventDecodeError : Json.Decode.Error -> UserNotice msg
eventDecodeError decodeErr =
    { uniqueLabel = "eventDecodeError" ++ (Json.Decode.errorToString decodeErr)
    , noticeType = Error
    , mainParagraphs = [ [ Element.text <| "Error decoding event: " ++ Json.Decode.errorToString decodeErr ] ]
    , align = BottomRight
    }


web3FetchError : String -> Http.Error -> UserNotice msg
web3FetchError label httpError =
    let
        _ =
            maybeDebugLog "http error for web3 fetch" httpError
    in
    { uniqueLabel = "web3FetchError " ++ label
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <|
                "Failed to fetch \""
                    ++ label
                    ++ "\"."
          ]
        ]
    , align = BottomRight
    }


web3SigError : String -> String -> UserNotice msg
web3SigError label errStr =
    { uniqueLabel = "web3SigError " ++ label
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "Error signing \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


web3BroadcastError : String -> String -> UserNotice msg
web3BroadcastError label errStr =
    { uniqueLabel = "web3BroadcastError " ++ label
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <| "Error broadcasting \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


web3MiningError : String -> String -> UserNotice msg
web3MiningError label errStr =
    { uniqueLabel = "web3MiningError " ++ label
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <| "Error mining \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


walletError : String -> UserNotice msg
walletError errStr =
    unexpectedError
        ("Error decoding JS walletSentry: " ++ errStr)
        Nothing


inputError : String -> UserNotice msg
inputError errStr =
    { uniqueLabel = "inputError" ++ errStr
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text errStr ] ]
    , align = BottomRight
    }


routeNotFound : UserNotice msg
routeNotFound =
    { uniqueLabel = "routeNotFound"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text "I don't understand that url..." ] ]
    , align = BottomRight
    }


debugMsg : String -> UserNotice msg
debugMsg s =
    { uniqueLabel = "debug"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "debug: " ++ s ] ]
    , align = BottomRight
    }
