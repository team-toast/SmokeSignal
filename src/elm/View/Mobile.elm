module View.Mobile exposing (navBar)

import Element exposing (Element, centerX, centerY, column, el, fill, height, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Theme exposing (orange)
import Types exposing (Model, Msg, View(..))


navBar : Model -> Element Msg
navBar model =
    [ viewNav model.view Types.ViewHome "home" "Home"
    , viewNav model.view Types.ViewTopics "label" "Topics"
    , viewNav model.view Types.ViewWallet "wallet" "Wallet"
    , viewNav model.view Types.ViewTxns "txn" "Txns"
    ]
        |> row
            [ width fill
            , Background.color black
            , Element.paddingXY 0 10
            ]


viewNav : View -> View -> String -> String -> Element Msg
viewNav curr view icon name =
    let
        active =
            case view of
                ViewHome ->
                    curr == ViewHome

                ViewTopics ->
                    case curr of
                        ViewTopics ->
                            True

                        ViewTopic _ ->
                            True

                        _ ->
                            False

                ViewWallet ->
                    curr == ViewWallet

                ViewTxns ->
                    curr == ViewTxns

                _ ->
                    False

        color =
            if active then
                orange

            else
                white
    in
    Input.button [ width fill ]
        { onPress = Just <| Types.GotoView view
        , label =
            [ Element.none
                |> el
                    [ Border.rounded 20
                    , height <| px 40
                    , width <| px 40
                    , Background.color color
                    , centerX
                    , Element.image
                        [ centerX
                        , centerY
                        , height <| px 30
                        ]
                        { src = "./img/icon/" ++ icon ++ ".svg"
                        , description = ""
                        }
                        |> Element.inFront
                    ]
            , text name
                |> el [ centerX, Font.color color ]
            ]
                |> column [ spacing 10, width fill ]
        }
