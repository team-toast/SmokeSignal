module View.Wallet exposing (view)

import Element exposing (Element, centerX, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Helpers.Element exposing (black, white)
import Theme
import Types exposing (Model, Msg)
import View.Attrs exposing (cappedWidth, hover, whiteGlowAttributeSmall)
import View.Sidebar


view : Model -> Element Msg
view model =
    if model.wallet == Types.NoneDetected then
        [ [ text "To post or interact with ", el [ Font.bold ] (text "SmokeSignal"), text ", you'll need a crypto identity." ]
            |> paragraph [ Font.center, Font.size 22 ]
        , [ text "On mobile you will need a wallet app with a DApp browser to accomplish this."
          ]
            |> paragraph [ Font.center, Font.size 22 ]
        , [ text "We recommend "
          , Element.newTabLink [ Font.bold, Font.color Theme.orange ]
                { url = "https://status.im/"
                , label = text "Status"
                }
          , text " and "
          , Element.newTabLink [ Font.bold, Font.color Theme.orange ]
                { url = "https://alphawallet.com/"
                , label = text "AlphaWallet"
                }
          , text " for the best xDai support."
          ]
            |> paragraph [ Font.center, Font.size 22 ]
        ]
            |> column
                [ padding 30
                , spacing 30
                , whiteGlowAttributeSmall
                , Background.color black
                , Font.color white
                , fill
                    |> Element.minimum 240
                    |> width
                ]

    else
        View.Sidebar.viewWallet model
