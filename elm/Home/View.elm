module Home.View exposing (view)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Utils
import Helpers.Element as EH exposing (changeForMobile)
import Theme exposing (..)
import Types exposing (..)
import Wallet exposing (Wallet)


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.paddingXY 20 80
            |> changeForMobile (Element.paddingXY 10 20) model.dProfile
        , Element.spacing (60 |> changeForMobile 15 model.dProfile)
        , Element.Font.color EH.white
        ]
        [ Element.column
            [ Element.centerX
            , Element.spacing 20
            ]
          <|
            [ Element.paragraph
                [ Element.Font.size 50
                , Element.Font.center
                ]
                [ Element.text "Freedom of Speech is being attacked." ]
            , Element.paragraph
                [ Element.Font.size 60
                , Element.Font.center
                , Element.Font.semiBold
                ]
                [ Element.text "SmokeSignal makes this futile." ]
            ]
        , Element.column
            [ Element.Border.rounded 15
            , Element.Background.color <| darkBlue
            , Element.padding 25
            , Element.Font.color <| EH.white
            , Element.Font.size 26
            , Element.Font.color <| Element.rgb 0.8 0.8 0.8
            , Element.width (Element.fill |> Element.maximum 800)
            , Element.centerX
            , Element.spacing 20
            ]
          <|
            List.map
                (Element.paragraph
                    [ Element.width Element.fill
                    , Element.Font.center
                    ]
                )
                [ [ Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat." ]
                , [ Element.text "No usernames. No moderators. No deplatforming." ]
                ]
        , actionRow model
        ]


actionRow : Model -> Element Msg
actionRow model =
    Element.row
        [ Element.width Element.fill
        , Element.spaceEvenly
        ]
        [ Element.text "todo"
        , composeActionBlock model
        , Element.text "todo"
        ]


composeActionBlock : Model -> Element Msg
composeActionBlock model =
    Element.column
        [ Element.spacing 20 ]
        [ homeWalletUX model.dProfile <|
            makeWalletUXPhaceInfo
                (Wallet.userInfo model.wallet)
                model.showAddress
                model.demoPhaceSrc
        ]


homeWalletUX : EH.DisplayProfile -> WalletUXPhaceInfo -> Element Msg
homeWalletUX dProfile walletUXPhaceInfo =
    Element.map MsgUp <|
        case walletUXPhaceInfo of
            DemoPhaceInfo demoAddress ->
                Element.column
                    [ Element.spacing 5
                    , Element.pointer
                    , Element.Events.onClick <| ConnectToWeb3
                    ]
                    [ phaceElement
                        False
                        MorphingPhace
                        (Eth.Utils.unsafeToAddress demoAddress)
                        False
                    ]

            -- Element.el commonAttributes <|
            UserPhaceInfo ( accountInfo, showAddress ) ->
                Element.el [] <|
                    phaceElement
                        True
                        UserPhace
                        accountInfo.address
                        showAddress
