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
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Maybe.Extra
import Post exposing (Post)
import Routing exposing (Route)
import Theme exposing (defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


viewFull : DisplayProfile -> WalletUXPhaceInfo -> Maybe PhaceIconId -> Model -> Element Msg
viewFull dProfile walletUXPhaceInfo showAddressId model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding (20 |> changeForMobile 10 dProfile)
        , Element.Background.color defaultTheme.appBackground
        ]
    <|
        view dProfile walletUXPhaceInfo showAddressId model


view : EH.DisplayProfile -> WalletUXPhaceInfo -> Maybe PhaceIconId -> Model -> Element Msg
view dProfile walletUXPhaceInfo showAddressId model =
    let
        commonAttributes =
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Background.color defaultTheme.postBodyBackground
            , composeUXShadow
            , Element.Border.rounded 10
            , Element.inFront <|
                EH.closeButton
                    [ Element.alignTop
                    , Element.alignRight
                    ]
                    (Element.rgb 0.3 0.3 0.3)
                    (MsgUp ExitCompose)
            ]
    in
    case dProfile of
        Desktop ->
            Element.row
                (commonAttributes
                    ++ [ Element.padding 20
                       , Element.spacing 20
                       ]
                )
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 10
                    ]
                    [ viewInput dProfile model.message
                    , Element.el [ Element.alignRight ] <| actionFormAndMaybeErrorEl dProfile walletUXPhaceInfo model
                    ]
                , viewPreviewWithPostContext dProfile Nothing model.renderedPreview model.context
                ]

        Mobile ->
            Element.column
                (commonAttributes
                    ++ [ Element.padding 10
                       , Element.spacing 10
                       , Element.Font.size 20
                       ]
                )
                (if model.showPreviewOnMobile then
                    [ viewPreviewWithPostContext
                        dProfile
                        (case Wallet.userInfo model.wallet of
                            Just userInfo ->
                                Just <|
                                    ( userInfo.address
                                    , showAddressId == Just PhaceForPreview
                                    )

                            Nothing ->
                                Nothing
                        )
                        model.renderedPreview
                        model.context
                    , actionFormAndMaybeErrorEl dProfile walletUXPhaceInfo model
                    ]

                 else
                    [ viewInput dProfile model.message
                    , viewPreviewButton
                        dProfile
                        (model.message /= "")
                    ]
                )


composeUXShadow : Attribute Msg
composeUXShadow =
    Element.Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = Element.rgba 0 0 0 1
        }


viewPreviewButton : DisplayProfile -> Bool -> Element Msg
viewPreviewButton dProfile enabled =
    if enabled then
        defaultTheme.secondaryActionButton
            dProfile
            []
            [ "Preview" ]
            MobilePreviewToggle

    else
        defaultTheme.disabledActionButton
            dProfile
            []
            "Preview"


viewInput : DisplayProfile -> String -> Element Msg
viewInput dProfile input =
    EH.scrollbarYEl [] <|
        Element.Input.multiline
            ([ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding (10 |> changeForMobile 5 dProfile)
            , Element.Background.color <| Element.rgba 1 1 1 0.5
            
            ] ++ (responsiveVal dProfile
                []
                [Element.Font.size 18])
            )
            { onChange = MessageInputChanged
            , text = input
            , placeholder = Just messageInputPlaceholder
            , label = Element.Input.labelHidden "messageInput"
            , spellcheck = True
            }


viewPreviewWithPostContext : DisplayProfile -> Maybe ( Address, Bool ) -> Maybe (Element Never) -> Post.Context -> Element Msg
viewPreviewWithPostContext dProfile maybeShowPhaceInfo renderedMessage context =
    EH.scrollbarYEl [] <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 15
            , Element.Background.color <| Element.rgba 1 1 1 0.5
            , Element.Border.width 1
            , Element.Border.color <| Element.rgba 0 0 0 0.5
            , Element.Border.rounded 10
            , Element.spacing 15
            ]
            [ Element.map MsgUp <|
                Element.row
                    [ Element.spacing 10
                    , Element.width Element.fill
                    ]
                    [ case maybeShowPhaceInfo of
                        Just ( fromAddress, showAddress ) ->
                            phaceElement True PhaceForPreview fromAddress showAddress

                        Nothing ->
                            Element.none
                    , Element.el [ Element.alignLeft ] <| viewContext context
                    ]
            , case renderedMessage of
                Nothing ->
                    appStatusMessage defaultTheme.appStatusTextColor "[Preview Box]"

                Just rendered ->
                    mapNever rendered
            ]


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


viewReplyInfo : Post.Id -> Element Msg
viewReplyInfo postId =
    Element.column
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 3
        ]
        [ Element.text "Replying to:"
        , Element.el
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                MsgUp <|
                    GotoRoute <|
                        Routing.ViewContext <|
                            Post.ForPost postId
            ]
            (Element.text <|
                shortenedHash postId.messageHash
            )
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
        , Element.scrollbarX
        , Element.clipX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "Topic:"
        , Element.el
            [ Element.Font.color defaultTheme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                MsgUp <|
                    GotoRoute <|
                        Routing.ViewContext <|
                            Post.ForTopic topic
            ]
            (Element.text <| topic)
        ]


actionFormAndMaybeErrorEl : EH.DisplayProfile -> WalletUXPhaceInfo -> Model -> Element Msg
actionFormAndMaybeErrorEl dProfile walletUXPhaceInfo model =
    case walletUXPhaceInfo of
        UserPhaceInfo ( userInfo, showAddress ) ->
            let
                ( goButtonEl, maybeErrorEls ) =
                    goButtonAndMaybeError dProfile userInfo model

                actionRow =
                    Element.row
                        [ Element.spacing 15
                        , Element.padding 10
                        , Element.Background.color <| Element.rgb 0.8 0.8 1
                        , Element.Border.rounded 10
                        ]
                        [ case dProfile of
                            Desktop ->
                                Element.map MsgUp <| phaceElement True UserPhace userInfo.address showAddress

                            Mobile ->
                                goBackButton dProfile
                        , inputsElement dProfile userInfo model
                        , goButtonEl
                        ]
            in
            case dProfile of
                Desktop ->
                    Element.row
                        [ Element.alignRight
                        , Element.spacing 10
                        ]
                        [ inputErrorEl dProfile maybeErrorEls
                        , actionRow
                        ]

                Mobile ->
                    Element.column
                        [ Element.centerX
                        , Element.spacing 5
                        ]
                        [ inputErrorEl dProfile maybeErrorEls
                        , actionRow
                        ]

        _ ->
            Element.map MsgUp <|
                web3ConnectButton dProfile [ Element.centerX, Element.centerY ]


inputsElement : EH.DisplayProfile -> UserInfo -> Model -> Element Msg
inputsElement dProfile userInfo model =
    Element.el
        ([ Element.centerY ]
            ++ (case dProfile of
                    Desktop ->
                        [ Element.width <| Element.px 260 ]

                    Mobile ->
                        []
               )
        )
    <|
        Element.el
            [ Element.centerX
            , Element.Font.size (20 |> changeForMobile 14 dProfile)
            ]
        <|
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
                                    [ Element.spacing (10 |> changeForMobile 5 dProfile)
                                    , Element.centerX
                                    ]
                                    [ Element.text "Burn"
                                    , burnAmountInput dProfile model.daiInput
                                    , Element.text "DAI"
                                    ]
                                , Element.row
                                    [ Element.Font.size (14 |> changeForMobile 10 dProfile)
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


inputErrorEl : DisplayProfile -> Maybe (List (Element Msg)) -> Element Msg
inputErrorEl dProfile els =
    let
        commonAttributes =
            [ Element.Font.color defaultTheme.errorTextColor
            , Element.Font.italic
            ]
    in
    case dProfile of
        Desktop ->
            Element.paragraph
                (commonAttributes
                    ++ [ Element.width (Element.fill |> Element.maximum 200)
                       , Element.alignTop
                       , Element.Font.alignRight
                       ]
                )
                (els |> Maybe.withDefault [ Element.text " " ])

        Mobile ->
            Element.paragraph
                (commonAttributes
                    ++ [ Element.width Element.fill
                       , Element.Font.size 14
                       ]
                )
                (els |> Maybe.withDefault [ Element.text " " ])


unlockButton : EH.DisplayProfile -> List (Attribute Msg) -> Element Msg
unlockButton dProfile attrs =
    defaultTheme.emphasizedActionButton
        dProfile
        attrs
        [ "Unlock Dai" ]
        (MsgUp UnlockDai)


burnAmountInput : DisplayProfile -> String -> Element Msg
burnAmountInput dProfile daiInput =
    Element.row []
        [ Element.Input.text
            [ Element.width <| Element.px (100 |> changeForMobile 60 dProfile)
            , Element.height <| Element.px (40 |> changeForMobile 35 dProfile)
            , Element.Font.size (20 |> changeForMobile 14 dProfile)
            , Element.Background.color <| Element.rgba 1 1 1 0.4
            ]
            { onChange = DaiInputChanged
            , text = daiInput
            , placeholder = Nothing
            , label = Element.Input.labelHidden "amount to burn"
            }
        ]


goButtonAndMaybeError : EH.DisplayProfile -> UserInfo -> Model -> ( Element Msg, Maybe (List (Element Msg)) )
goButtonAndMaybeError dProfile userInfo model =
    case userInfo.balance of
        Just balance ->
            if TokenValue.isZero balance then
                ( maybeGoButton dProfile Nothing
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
                                    ( maybeGoButton dProfile Nothing
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
                                    let
                                        maybeUpToDateRender =
                                            if model.renderNeeded then
                                                Nothing

                                            else
                                                model.renderedPreview
                                    in
                                    case (validateResults.message, maybeUpToDateRender) of
                                        (Just message, Just rendered) ->
                                            ( maybeGoButton dProfile <|
                                                Just <|
                                                    Post.Draft
                                                        donateAmount
                                                        (Post
                                                            userInfo.address
                                                            burnAmount
                                                            message
                                                            (Post.buildMetadataFromContext model.context)
                                                            rendered
                                                        )
                                            , Nothing
                                            )

                                        _ ->
                                            ( maybeGoButton dProfile Nothing
                                            , Nothing
                                            )

                            Just (Err errStr) ->
                                ( maybeGoButton dProfile Nothing
                                , Just [ Element.text errStr ]
                                )

                            Nothing ->
                                ( maybeGoButton dProfile Nothing
                                , Nothing
                                )

                    _ ->
                        ( maybeGoButton dProfile Nothing
                        , Nothing
                        )

        _ ->
            ( maybeGoButton dProfile Nothing
            , Nothing
            )


commonActionButtonStyles : DisplayProfile -> List (Attribute Msg)
commonActionButtonStyles dProfile =
    [ Element.height <| Element.px (100 |> changeForMobile 70 dProfile)
    , Element.width <| Element.px (100 |> changeForMobile 70 dProfile)
    , Element.Font.size (26 |> changeForMobile 20 dProfile)
    , Element.Border.rounded (10 |> changeForMobile 7 dProfile)
    ]


maybeGoButton : EH.DisplayProfile -> Maybe Post.Draft -> Element Msg
maybeGoButton dProfile maybeDraft =
    case maybeDraft of
        Just draft ->
            defaultTheme.emphasizedActionButton
                dProfile
                (commonActionButtonStyles dProfile)
                [ "GO" ]
                (MsgUp <| SubmitPost draft)

        Nothing ->
            defaultTheme.disabledActionButton
                dProfile
                (commonActionButtonStyles dProfile)
                "GO"


goBackButton : EH.DisplayProfile -> Element Msg
goBackButton dProfile =
    defaultTheme.secondaryActionButton
        dProfile
        (commonActionButtonStyles dProfile)
        [ "Edit" ]
        MobilePreviewToggle


mapNever : Element Never -> Element Msg
mapNever =
    Element.map (always <| MsgUp NoOp)
