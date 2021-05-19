module View.Phace exposing (view)

import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , row
        , spacingXY
        , text
        , width
        , paddingXY
        )
import Types exposing (Model, Msg)
import View.Common exposing (phaceElement)
import Eth.Utils
import Types exposing (PhaceIconId(..))
import Types exposing (Msg(..))


view : Model -> Element Msg
view model =
    let
        isMobile =
            model.dProfile == Types.Mobile
    in
    row
        [ width fill
        , spacingXY 0 20
        ]
        [ column
            [ width fill ]
            [ el [ centerX ] <| text "savedPhaces"
            ]
        , column
            [ width fill ]
            [ row
                [ width fill ]
                [ column []
                 [ phaceElement
                        90
                        (Eth.Utils.unsafeToAddress model.demoPhaceSrc)
                        (model.showAddressId == Just DemoPhace)
                        (ShowOrHideAddress DemoPhace)]
                , column [ paddingXY 20 10, spacingXY 10 10]
                    [ row
                    []
                    [ el [centerX] <| text "Cycle Phace"]
                    ,
                    row
                    []
                    [ el [centerX] <| text "SavePhace"]
                    ]
                ]
            , row [ width fill ]
                [ el [ centerX ] <| text "Ethereum Address"
                ]
            ]
        ]
