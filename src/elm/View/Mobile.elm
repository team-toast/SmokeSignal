module View.Mobile exposing (navBar)

import Dict
import Element exposing (Element, centerX, centerY, column, el, fill, height, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (black, white)
import Theme exposing (orange)
import Types exposing (Model, Msg, View(..))
import View.Common exposing (whenAttr, whenJust)
import View.Modal


navBar : Model -> Element Msg
navBar model =
    let
        txDot =
            Dict.size model.trackedTxs
                |> (\n ->
                        if n == 0 then
                            Nothing

                        else
                            Just n
                   )
    in
    [ viewNav model.view Types.ViewHome "home" "Home" Nothing
    , viewNav model.view Types.ViewTopics "label" "Topics" Nothing
    , viewNav model.view Types.ViewWallet "wallet" "Wallet" Nothing
    , viewNav model.view Types.ViewTxns "txn" "Txns" txDot
    ]
        |> row
            [ width fill
            , Background.color black
            , View.Modal.viewCookieConsent True
                |> Element.above
                |> View.Common.whenAttr (not model.cookieConsentGranted)
            ]


viewNav : View -> View -> String -> String -> Maybe Int -> Element Msg
viewNav curr view icon name dot =
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
                |> column
                    [ spacing 10
                    , width fill
                    , dot
                        |> whenJust
                            (String.fromInt
                                >> text
                                >> el [ centerY, centerX ]
                                >> el
                                    [ width <| px 25
                                    , height <| px 25
                                    , Font.size 17
                                    , Font.bold
                                    , Background.color Theme.green
                                    , Border.rounded 15
                                    ]
                                >> el
                                    [ paddingXY 15 0
                                    , Element.alignTop
                                    , Element.alignRight
                                    ]
                            )
                        |> Element.inFront
                        |> whenAttr (dot /= Nothing)
                    , Element.paddingXY 0 10
                    ]
        }
