module View.Phace exposing (view)

import DemoPhaceSrcMutator
import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , padding
        , paddingXY
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Eth.Types exposing (Address)
import Eth.Utils
import Html.Events exposing (onClick)
import Maybe.Extra exposing (unwrap)
import Random
import Theme exposing (white)
import Types exposing (Model, Msg(..), PhaceIconId(..))
import View.Attrs
import View.Common exposing (phaceElement)
import Wallet


view : Model -> Element Msg
view model =
    let
        isMobile =
            model.dProfile == Types.Mobile

        phaceEl =
            model.wallet
                |> Wallet.userInfo
                |> unwrap
                    (phaceElement
                        90
                        (Eth.Utils.unsafeToAddress model.demoPhaceSrc)
                        (model.showAddressId == Just DemoPhace)
                        (ShowOrHideAddress DemoPhace)
                    )
                    (\userInfo ->
                        phaceElement
                            90
                            userInfo.address
                            (model.showAddressId == Just UserPhace)
                            (ShowOrHideAddress UserPhace)
                    )
    in
    row
        [ width fill
        , spacingXY 0 20
        ]
        [ column
            [ width fill ]
            [ row
                [ width fill, spacing 10, padding 10 ]
                [ column []
                    [ phaceEl ]
                , column [ paddingXY 20 10, spacing 10 ]
                    [ row
                        []
                        [ viewCyclePhaceButton ]
                    , row
                        []
                        [ viewSavePhaceButton ]
                    ]
                ]
            , el [ centerX, Font.color white ] <| text model.inDappWalletAddress
            ]
        ]


viewCyclePhaceButton : Element Msg
viewCyclePhaceButton =
    Input.button
        [ centerX
        , padding 10
        , View.Attrs.roundBorder
        , Background.color Theme.green
        , Font.color white
        ]
        { onPress = Just Types.ConnectToInDappWallet
        , label = text "Cycle Phace"
        }


viewSavePhaceButton : Element Msg
viewSavePhaceButton =
    Input.button
        [ centerX
        , padding 10
        , View.Attrs.roundBorder
        , Background.color Theme.green
        , Font.color white
        ]
        { onPress = Just Types.CyclePhace
        , label = text "Save Phace"
        }
