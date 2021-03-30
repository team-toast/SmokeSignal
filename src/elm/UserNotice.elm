module UserNotice exposing (Alignment(..), NoticeType(..), UserNotice, cantConnectNoWeb3, debugMsg, eventDecodeError, faucetRequestSuccessful, inputError, noWeb3Account, noWeb3Provider, notify, routeNotFound, screenTooSmall, unexpectedError, walletError, web3BroadcastError, web3FetchError, web3MiningError, web3SigError, wrongWeb3Network)

import Element exposing (Element)
import Element.Font
import Json.Decode


type alias UserNotice =
    { noticeType : NoticeType
    , mainParagraphs : List (List (Element Never))
    , align : Alignment
    , uniqueLabel : String
    }


type Alignment
    = BottomRight
    | TopLeft


type NoticeType
    = Update
    | Caution
    | Error
    | ShouldBeImpossible


screenTooSmall : Int -> UserNotice
screenTooSmall width =
    { uniqueLabel = "screenTooSmall " ++ String.fromInt width
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "Your screen is quite small (" ++ String.fromInt width ++ " across)--things may be broken!." ] ]
    , align = TopLeft
    }


noWeb3Provider : UserNotice
noWeb3Provider =
    { uniqueLabel = "noWeb3Provider"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "No web3 provider detected. Is "
          , Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                { url = "https://metamask.io/"
                , label = Element.text "MetaMask"
                }
          , Element.text " or some other web3 provider installed and unlocked?"
          ]
        ]
    , align = BottomRight
    }


noWeb3Account : UserNotice
noWeb3Account =
    { uniqueLabel = "noWeb3Account"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "I can't detect a web3 account. Your wallet may be locked." ]
        ]
    , align = BottomRight
    }


cantConnectNoWeb3 : UserNotice
cantConnectNoWeb3 =
    { uniqueLabel = "cantConnectNoWeb3"
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text "You need a web3 provider (such as "
          , Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                { url = "https://metamask.io/"
                , label = Element.text "MetaMask"
                }
          , Element.text ") to Connect."
          ]
        , [ Element.text "Until you connect, SmokeSignal will operate in read-only mode." ]
        ]
    , align = BottomRight
    }


wrongWeb3Network : UserNotice
wrongWeb3Network =
    { uniqueLabel = "wrongWeb3Network"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text "SmokeSignal only works on Ethereum mainnet. Make sure your wallet is set to Ethereum mainnet." ]
        ]
    , align = BottomRight
    }


notify : String -> UserNotice
notify text =
    { uniqueLabel = text
    , noticeType = Update
    , mainParagraphs = [ [ Element.text text ] ]
    , align = BottomRight
    }


unexpectedError : String -> UserNotice
unexpectedError text =
    { uniqueLabel = "unexpectedError " ++ text
    , noticeType = ShouldBeImpossible
    , mainParagraphs = [ [ Element.text text ] ]
    , align = BottomRight
    }


eventDecodeError : Json.Decode.Error -> UserNotice
eventDecodeError decodeErr =
    { uniqueLabel = "eventDecodeError" ++ Json.Decode.errorToString decodeErr
    , noticeType = Error
    , mainParagraphs = [ [ Element.text <| "Error decoding event: " ++ Json.Decode.errorToString decodeErr ] ]
    , align = BottomRight
    }


web3FetchError : String -> UserNotice
web3FetchError label =
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


web3SigError : String -> String -> UserNotice
web3SigError label errStr =
    { uniqueLabel = "web3SigError " ++ label
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "Error signing \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


web3BroadcastError : String -> String -> UserNotice
web3BroadcastError label errStr =
    { uniqueLabel = "web3BroadcastError " ++ label
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <| "Error broadcasting \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


web3MiningError : String -> String -> UserNotice
web3MiningError label errStr =
    { uniqueLabel = "web3MiningError " ++ label
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <| "Error mining \"" ++ label ++ "\" transaction: " ++ errStr ] ]
    , align = BottomRight
    }


walletError : String -> UserNotice
walletError errStr =
    unexpectedError
        ("Error decoding JS walletSentry: " ++ errStr)


inputError : String -> UserNotice
inputError errStr =
    { uniqueLabel = "inputError" ++ errStr
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text errStr ] ]
    , align = BottomRight
    }


routeNotFound : Maybe String -> UserNotice
routeNotFound maybeErrStr =
    { uniqueLabel = "routeNotFound"
    , noticeType = Error
    , mainParagraphs =
        [ [ Element.text <|
                "I don't understand that url"
                    ++ (case maybeErrStr of
                            Just errStr ->
                                ": " ++ errStr

                            Nothing ->
                                ""
                       )
          ]
        ]
    , align = BottomRight
    }


debugMsg : String -> UserNotice
debugMsg s =
    { uniqueLabel = "debug" ++ s
    , noticeType = Caution
    , mainParagraphs =
        [ [ Element.text <| "debug: " ++ s ] ]
    , align = BottomRight
    }


faucetRequestSuccessful : UserNotice
faucetRequestSuccessful =
    { noticeType = Update
    , mainParagraphs =
        [ [ Element.text "Faucet request successful! The balance should update in your wallet soon." ] ]
    , align = BottomRight
    , uniqueLabel = "faucet request successful"
    }
