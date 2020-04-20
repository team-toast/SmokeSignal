module ComposeUX.View exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import ComposeUX.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Utils
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Maybe.Extra
import Post
import Routing exposing (Route)
import Theme exposing (defaultTheme)
import TokenValue exposing (TokenValue)


viewFull : EH.DisplayProfile -> WalletUXPhaceInfo -> Model -> Maybe String -> Element Msg
viewFull dProfile walletUXPhaceInfo model maybeTopic =
    Debug.todo ""


viewHalf : EH.DisplayProfile -> WalletUXPhaceInfo -> Model -> String -> Element Msg
viewHalf dProfile walletUXPhaceInfo model topic =
    Element.column
        [ Element.width Element.fill
        , Element.Background.color defaultTheme.postBodyBackground
        , Element.padding 10
        , composeUXShadow
        , Element.Border.roundEach
            { topLeft = 0
            , topRight = 10
            , bottomRight = 10
            , bottomLeft = 10
            }
        , Element.mapAttribute MsgUp <|
            Element.above <|
                Element.el
                    [ Element.alignLeft
                    ]
                <|
                    defaultTheme.secondaryActionButton
                        EH.Mobile
                        []
                        [ "Hide" ]
                        (ShowHalfComposeUX False)
        ]
        [ viewComposeMetadata model.replyTo topic
        , viewMessageInputBox model.message
        , actionFormAndMaybeErrorEl dProfile walletUXPhaceInfo model topic
        ]


composeUXShadow : Attribute Msg
composeUXShadow =
    Element.Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = Theme.darkGray
        }


viewComposeMetadata : Maybe Post.Id -> String -> Element Msg
viewComposeMetadata maybeReplyTo topic =
    Maybe.map
        (Element.row
            [ Element.spacing 10
            , Element.paddingXY 30 10
            , Element.width Element.fill
            ]
        )
        ([ Maybe.map
            (viewReplyInfo (Just <| UpdateReplyTo Nothing))
            maybeReplyTo
         , Just <| Element.el [ Element.alignRight ] <| viewTopic topic
         ]
            |> Maybe.Extra.values
            |> ListHelpers.nonEmpty
        )
        |> Maybe.withDefault Element.none


viewMessageInputBox : String -> Element Msg
viewMessageInputBox input =
    Element.el
        [ Element.width Element.fill
        , Element.padding 10
        , Element.Border.roundEach
            { bottomRight = 0
            , bottomLeft = 10
            , topRight = 10
            , topLeft = 10
            }
        , Element.Background.color defaultTheme.postBodyBackground
        ]
    <|
        Element.Input.multiline
            [ Element.width Element.fill
            , Element.height (Element.px 300)
            , Element.Background.color <| Element.rgba 1 1 1 0.5
            ]
            { onChange = MessageInputChanged
            , text = input
            , placeholder = Just messageInputPlaceholder
            , label = Element.Input.labelHidden "messageInput"
            , spellcheck = True
            }


messageInputPlaceholder : Element.Input.Placeholder Msg
messageInputPlaceholder =
    Element.Input.placeholder [] <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
        <|
            List.map
                (Element.paragraph
                    [ Element.Font.color defaultTheme.messageInputPlaceholderTextColor
                    , Element.Font.italic
                    ]
                    << List.map Element.text
                )
                [ [ "SmokeSignal messages are formatted with markdown (e.g. *italic*, **bold**, [link-title](url))." ]
                , [ "Hackmd.io is useful for drafting and previewing markdown text." ]
                ]


viewReplyInfo : Maybe Msg -> Post.Id -> Element Msg
viewReplyInfo maybeCloseMsg postId =
    Element.row
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        ]
        [ Element.column
            [ Element.spacing 3
            ]
            [ Element.text "Replying to:"
            , Element.el
                [ Element.Font.color defaultTheme.linkTextColor
                , Element.pointer
                , Element.Events.onClick <|
                    MsgUp <|
                        GotoRoute <|
                            Routing.ViewPost postId
                ]
                (Element.text <|
                    shortenedHash postId.messageHash
                )
            ]
        , case maybeCloseMsg of
            Just closeMsg ->
                Element.image
                    [ Element.padding 4
                    , Element.alignTop
                    , Element.pointer
                    , Element.Events.onClick closeMsg
                    ]
                    { src = "img/remove-circle-black.svg"
                    , description = "remove"
                    }

            Nothing ->
                Element.none
        ]


viewTopic : String -> Element Msg
viewTopic topic =
    Element.column
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        , Element.clipX
        , Element.scrollbarX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "topic:"
        , Element.el
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                MsgUp <|
                    GotoRoute <|
                        Routing.ViewTopic topic
            ]
            (Element.text topic)
        ]


actionFormAndMaybeErrorEl : EH.DisplayProfile -> WalletUXPhaceInfo -> Model -> String -> Element Msg
actionFormAndMaybeErrorEl dProfile walletUXPhaceInfo model topic =
    case walletUXPhaceInfo of
        UserPhaceInfo ( userInfo, showAddress ) ->
            let
                ( goButtonEl, maybeErrorEls ) =
                    previewButtonAndMaybeError dProfile userInfo model topic
            in
            Element.row
                [ Element.alignLeft
                , Element.spacing 10
                ]
                [ Element.row
                    [ Element.spacing 15
                    , Element.padding 10
                    , Element.Background.color <| Element.rgb 0.8 0.8 1
                    , Element.Border.rounded 10
                    ]
                    [ Element.map MsgUp <| phaceElement True UserPhace userInfo.address showAddress
                    , inputsElement dProfile userInfo model
                    , goButtonEl
                    ]
                , inputErrorEl maybeErrorEls
                ]

        _ ->
            Element.map MsgUp <|
                web3ConnectButton dProfile [ Element.centerX, Element.centerY ]


inputsElement : EH.DisplayProfile -> UserInfo -> Model -> Element Msg
inputsElement dProfile userInfo model =
    Element.el
        [ Element.centerY
        , Element.width <| Element.px 260
        ]
    <|
        Element.el [ Element.centerX ] <|
            case model.miningUnlockTx of
                Just txHash ->
                    Element.column
                        [ Element.spacing 4
                        , Element.Font.color (Element.rgb 0.3 0.3 0.3)
                        , Element.centerY
                        ]
                        [ Element.el
                            [ Element.centerX ]
                            (Element.text "Mining DAI unlock tx...")
                        , Element.newTabLink
                            [ Element.Font.color defaultTheme.linkTextColor
                            , Element.Font.size 14
                            , Element.centerX
                            ]
                            { url = EthHelpers.etherscanTxUrl txHash
                            , label = Element.text "(track on etherscan)"
                            }
                        , Element.el
                            [ Element.centerX ]
                            (Element.text "Feel free to draft your message")
                        , Element.el
                            [ Element.centerX ]
                            (Element.text "while waiting!")
                        ]

                Nothing ->
                    case userInfo.daiUnlocked of
                        Nothing ->
                            loadingElement
                                [ Element.centerX
                                , Element.centerY
                                ]
                            <|
                                Just "Checking DAI lock..."

                        Just False ->
                            unlockButton
                                dProfile
                                [ Element.centerX
                                , Element.centerY
                                ]

                        Just True ->
                            Element.column
                                [ Element.spacing 10 ]
                                [ Element.row
                                    [ Element.spacing 10
                                    , Element.centerX
                                    ]
                                    [ Element.text "Burn"
                                    , burnAmountInput model.daiInput
                                    , Element.text "DAI"
                                    ]
                                , Element.row
                                    [ Element.Font.size 14
                                    , Element.spacing 5
                                    ]
                                    [ Element.Input.checkbox [ Element.alignTop ]
                                        { onChange = DonationCheckboxSet
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = model.donateChecked
                                        , label = Element.Input.labelHidden "Donate an extra 1% to Foundry"
                                        }
                                    , Element.column
                                        [ Element.spacing 5 ]
                                        [ Element.row []
                                            [ Element.text "Donate an extra 1% to "
                                            , Element.newTabLink
                                                [ Element.Font.color defaultTheme.linkTextColor ]
                                                { url = "https://foundrydao.com/"
                                                , label = Element.text "Foundry"
                                                }
                                            ]
                                        , Element.text "so we can build more cool stuff!"
                                        ]
                                    ]
                                ]


inputErrorEl : Maybe (List (Element Msg)) -> Element Msg
inputErrorEl =
    Maybe.map
        (Element.paragraph
            [ Element.width (Element.fill |> Element.maximum 700)
            , Element.Font.color defaultTheme.errorTextColor
            , Element.Font.italic
            ]
        )
        >> Maybe.withDefault Element.none


unlockButton : EH.DisplayProfile -> List (Attribute Msg) -> Element Msg
unlockButton dProfile attrs =
    defaultTheme.emphasizedActionButton
        dProfile
        attrs
        [ "Unlock Dai" ]
        (MsgUp UnlockDai)


burnAmountInput : String -> Element Msg
burnAmountInput daiInput =
    Element.row []
        [ Element.Input.text
            [ Element.width <| Element.px 100
            , Element.Background.color <| Element.rgba 1 1 1 0.4
            ]
            { onChange = DaiInputChanged
            , text = daiInput
            , placeholder = Nothing
            , label = Element.Input.labelHidden "amount to burn"
            }
        ]


previewButtonAndMaybeError : EH.DisplayProfile -> UserInfo -> Model -> String -> ( Element Msg, Maybe (List (Element Msg)) )
previewButtonAndMaybeError dProfile userInfo model topic =
    case userInfo.balance of
        Just balance ->
            if TokenValue.isZero balance then
                ( maybePreviewButton dProfile Nothing
                , Just
                    [ Element.text <|
                        "That account ("
                            ++ Eth.Utils.addressToChecksumString userInfo.address
                            ++ ") doesn't have any DAI! "
                    , Element.newTabLink [ Element.Font.color defaultTheme.linkTextColor ]
                        { url = "https://kyberswap.com/swap/eth-dai"
                        , label = Element.text "Kyberswap"
                        }
                    , Element.text " can swap your ETH for DAI in a single transaction."
                    ]
                )

            else
                case userInfo.daiUnlocked of
                    Just True ->
                        let
                            validateResults =
                                validateInputs model
                        in
                        case validateResults.burnAndDonateAmount of
                            Just (Ok ( burnAmount, donateAmount )) ->
                                let
                                    balanceTooLow =
                                        TokenValue.compare
                                            (TokenValue.add burnAmount donateAmount)
                                            balance
                                            == GT
                                in
                                if balanceTooLow then
                                    ( maybePreviewButton dProfile Nothing
                                    , Just
                                        [ Element.text "You don't have that much DAI in your wallet! "
                                        , Element.newTabLink [ Element.Font.color defaultTheme.linkTextColor ]
                                            { url = "https://kyberswap.com/swap/eth-dai"
                                            , label = Element.text "Kyberswap"
                                            }
                                        , Element.text " can swap your ETH for DAI in a single transaction."
                                        ]
                                    )

                                else
                                    case validateResults.message of
                                        Just message ->
                                            ( maybePreviewButton dProfile <|
                                                Just <|
                                                    Post.Draft
                                                        userInfo.address
                                                        message
                                                        burnAmount
                                                        donateAmount
                                                        (Post.versionedMetadata
                                                            validateResults.replyTo
                                                            (Just topic)
                                                        )
                                            , Nothing
                                            )

                                        Nothing ->
                                            ( maybePreviewButton dProfile Nothing
                                            , Nothing
                                            )

                            Just (Err errStr) ->
                                ( maybePreviewButton dProfile Nothing
                                , Just [ Element.text errStr ]
                                )

                            Nothing ->
                                ( maybePreviewButton dProfile Nothing
                                , Nothing
                                )

                    _ ->
                        ( maybePreviewButton dProfile Nothing
                        , Nothing
                        )

        _ ->
            ( maybePreviewButton dProfile Nothing
            , Nothing
            )


maybePreviewButton : EH.DisplayProfile -> Maybe Post.Draft -> Element Msg
maybePreviewButton dProfile maybeDraft =
    let
        commonStyles =
            [ Element.height <| Element.px 100
            , Element.width <| Element.px 100
            , Element.Font.size 26
            , Element.Border.rounded 10
            ]
    in
    case maybeDraft of
        Just draft ->
            defaultTheme.emphasizedActionButton
                dProfile
                commonStyles
                [ "Preview" ]
                (MsgUp <| SubmitPost draft)

        Nothing ->
            defaultTheme.disabledActionButton
                dProfile
                commonStyles
                "Preview"