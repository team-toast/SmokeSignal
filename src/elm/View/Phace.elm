module View.Phace exposing (view)

import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , paddingXY
        , row
        , spacingXY
        , text
        , width
        ,padding
        )
import Element.Events
import Eth.Utils
import Html.Events exposing (onClick)
import Maybe.Extra exposing (unwrap)
import Types exposing (Model, Msg(..), PhaceIconId(..))
import View.Common exposing (phaceElement)
import Wallet
import Element.Font exposing (Font)
import Theme exposing (white)


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
            [ el [ centerX, Element.Font.color white ] <| text "savedPhaces"
            ]
        , column
            [ width fill ]
            [ row
                [ width fill ]
                [ column []
                    [ phaceEl ]
                , column [ paddingXY 20 10, spacingXY 10 10 ]
                    [ row
                        []
                        [ el [ centerX, Element.Font.color white, Element.Events.onClick CyclePhace ] <| text "Cycle Phace" ]
                    , row
                        []
                        [ el [ centerX, Element.Font.color white ] <| text "SavePhace" ]
                    ]
                ]
            , row [ padding 20 ]
                [ el [ centerX, Element.Font.color white ] <| text "Ethereum Address"
                ]
            ]
        ]
