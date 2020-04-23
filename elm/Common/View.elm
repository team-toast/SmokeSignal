module Common.View exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Phace
import Theme exposing (defaultTheme)
import Time


shortenedHash : Hex -> String
shortenedHash hash =
    let
        hashStr =
            Eth.Utils.hexToString hash
    in
    if String.length hashStr <= 10 then
        hashStr

    else
        String.left 6 hashStr
            ++ "..."
            ++ String.right 4 hashStr


web3ConnectButton : EH.DisplayProfile -> List (Attribute MsgUp) -> Element MsgUp
web3ConnectButton dProfile attrs =
    defaultTheme.emphasizedActionButton
        dProfile
        attrs
        [ "Connect to Wallet" ]
        ConnectToWeb3


phaceElement : Bool -> PhaceIconId -> Address -> Bool -> Element MsgUp
phaceElement addressHangToRight phaceId fromAddress showAddress =
    let
        addressOutputEl =
            Element.el
                [ Element.alignBottom
                , if addressHangToRight then
                    Element.alignLeft

                  else
                    Element.alignRight
                , Element.Background.color EH.white
                , Element.Font.size 12
                , EH.moveToFront
                , Element.Border.width 2
                , Element.Border.color EH.black
                ]
                (Element.text <| Eth.Utils.addressToChecksumString fromAddress)
    in
    Element.el
        (if showAddress then
            [ Element.inFront addressOutputEl
            , Element.alignTop
            ]

         else
            [ Element.alignTop ]
        )
    <|
        Element.el
            [ Element.Border.rounded 10
            , Element.clip
            , Element.pointer
            , EH.onClickNoPropagation (ShowOrHideAddress phaceId)
            , Element.Border.width 2
            , Element.Border.color EH.black
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress)


loadingElement : List (Attribute msg) -> Maybe String -> Element msg
loadingElement attrs maybeString =
    Element.el
        ([ Element.Font.italic
         , Element.Font.color defaultTheme.loadingTextColor
         , Element.Font.size 20
         ]
            ++ attrs
        )
        (Element.text <| Maybe.withDefault "loading..." maybeString)


walletUX : EH.DisplayProfile -> Bool -> WalletUXPhaceInfo -> Element MsgUp
walletUX dProfile addressHangToRight walletUXPhaceInfo =
    let
        commonAttributes =
            [-- Element.alignRight
             -- , Element.alignTop
             -- , Element.padding 10
             -- , Element.Border.roundEach
             --     { bottomLeft = 10
             --     , topLeft = 0
             --     , topRight = 0
             --     , bottomRight = 0
             --     }
             -- , commonShadow
             -- , Element.Background.color blue
             -- , Element.Border.color (Element.rgba 0 0 1 0.5)
             -- , Element.Border.widthEach
             --     { top = 1
             --     , right = 1
             --     , bottom = 0
             --     , left = 0
             --     }
            ]
    in
    case walletUXPhaceInfo of
        DemoPhaceInfo demoAddress ->
            Element.column
                (commonAttributes
                    ++ [ Element.spacing 5 ]
                )
                [ Element.el
                    [ Element.inFront <|
                        Element.el
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.Background.color <| Element.rgba 0 0 0 0.4
                            , Element.Border.rounded 10
                            , Element.pointer
                            , Element.Events.onClick <|
                                ConnectToWeb3
                            ]
                        <|
                            Element.el
                                [ Element.alignBottom
                                , Element.width Element.fill
                                , Element.Background.color <| Element.rgba 0 0 0 0.4
                                , Element.Font.color EH.white
                                , Element.Font.bold
                                , Element.Font.size 14
                                ]
                            <|
                                Element.text "Connect Wallet"
                    ]
                  <|
                    phaceElement
                        addressHangToRight
                        MorphingPhace
                        (Eth.Utils.unsafeToAddress demoAddress)
                        False
                ]

        -- Element.el commonAttributes <|
        UserPhaceInfo ( accountInfo, showAddress ) ->
            Element.el commonAttributes <|
                phaceElement
                    addressHangToRight
                    UserPhace
                    accountInfo.address
                    showAddress


emphasizedText : String -> Element msg
emphasizedText =
    Element.el
        [ Element.Font.bold
        , Element.Font.color EH.white
        ]
        << Element.text


daiSymbol : Bool -> List (Attribute msg) -> Element msg
daiSymbol isWhite attributes =
    Element.image attributes
        { src =
            if isWhite then
                "img/dai-unit-char-white.svg"

            else
                "img/dai-unit-char-black.svg"
        , description = ""
        }


appStatusMessage : Element.Color -> String -> Element msg
appStatusMessage color errStr =
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.paragraph
            [ Element.centerX
            , Element.centerY
            , Element.Font.center
            , Element.Font.italic
            , Element.Font.color color
            , Element.Font.size 36
            , Element.width (Element.fill |> Element.maximum 800)
            , Element.padding 40
            ]
            [ Element.text errStr ]


posixToString : Time.Posix -> String
posixToString t =
    let
        z =
            Time.utc
    in
    String.fromInt (Time.toYear z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt <| TimeHelpers.monthToInt <| Time.toMonth z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toDay z t))
        ++ " "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour z t))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute z t))
        ++ " (UTC)"


subheaderAttributes : List (Attribute msg)
subheaderAttributes =
    [ Element.paddingXY 0 20
    , Element.Font.size 50
    , Element.Font.color defaultTheme.headerTextColor
    ]


subheaderButtonAttributes : List (Attribute msg)
subheaderButtonAttributes =
    [ Element.paddingXY 30 10
    , Element.centerX
    ]
