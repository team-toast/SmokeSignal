module View exposing (root)

import Browser
import CommonTypes exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Lazy
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Html.Attributes
import Markdown
import Phace
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet


root : Model -> Browser.Document Msg
root model =
    { title = "SmokeSignal"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.Events.onClick ClickHappened
            ]
          <|
            body model
        ]
    }


body : Model -> Element Msg
body model =
    Element.column
        ([ Element.width Element.fill
         , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
         , Element.inFront <|
            if model.showComposeUX then
                Element.none

            else
                viewMinimizedComposeUX
                    (Wallet.userInfo model.wallet
                        |> Maybe.map
                            (\userInfo ->
                                ( userInfo
                                , model.showAddress == Just User
                                )
                            )
                    )
         ]
            ++ List.map
                Element.inFront
                (userNoticeEls Desktop model.userNotices)
        )
        [ title
        , Element.Lazy.lazy4
            viewMessagesAndDrafts
            model.blockTimes
            model.messages
            model.miningMessages
            model.showAddress
        , if model.showComposeUX then
            Element.el
                [ Element.width Element.fill
                , Element.alignBottom
                ]
                (viewComposeUX
                    (Wallet.userInfo model.wallet
                        |> Maybe.map
                            (\userInfo ->
                                ( userInfo
                                , model.showAddress == Just User
                                )
                            )
                    )
                    model.composeUXModel
                )

          else
            Element.none
        ]


title : Element Msg
title =
    Element.el
        [ Element.Font.size 40
        , Element.Font.bold
        , Element.padding 30
        ]
    <|
        Element.text "SmokeSignal"


viewMinimizedComposeUX : Maybe ( UserInfo, Bool ) -> Element Msg
viewMinimizedComposeUX maybeUserInfoAndShowAddress =
    let
        commonAttributes =
            [ Element.alignLeft
            , Element.alignBottom
            , Element.Background.color EH.lightBlue
            , Element.padding 10
            , Element.Border.roundEach
                { topLeft = 0
                , topRight = 10
                , bottomRight = 0
                , bottomLeft = 0
                }
            , composeUXShadow
            , Element.Border.color (Element.rgba 0 0 1 0.5)
            , Element.Border.widthEach
                { top = 1
                , right = 1
                , bottom = 0
                , left = 0
                }
            ]
    in
    case maybeUserInfoAndShowAddress of
        Nothing ->
            Element.el commonAttributes <|
                web3ConnectButton []

        Just ( accountInfo, showAddress ) ->
            Element.column
                (commonAttributes
                    ++ [ Element.spacing 3
                       ]
                )
            <|
                [ phaceElement
                    User
                    accountInfo.address
                    showAddress
                , EH.blueButton
                    Mobile
                    [ Element.width Element.fill ]
                    [ "Compose" ]
                    (ShowComposeUX True)
                ]


composeUXShadow : Attribute Msg
composeUXShadow =
    Element.Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = EH.darkGray
        }


viewMessagesAndDrafts : Dict Int Time.Posix -> Dict Int (List Message) -> Dict String MiningMessage -> Maybe PhaceId -> Element Msg
viewMessagesAndDrafts blockTimes messages miningMessages maybeShowAddressForPhace =
    if Dict.isEmpty messages then
        Element.el
            [ Element.centerX
            , Element.centerY
            , Element.Font.size 36
            , Element.Font.color EH.darkGray
            , Element.Font.italic
            ]
            (Element.text "Searching for SmokeSignal messages...")

    else
        let
            messagesList =
                messages
                    |> Dict.map
                        (\blocknum messagesForBlock ->
                            Element.column
                                [ Element.paddingXY 20 0 ]
                                (List.map
                                    (viewMessage
                                        (case maybeShowAddressForPhace of
                                            Just (MinedMessage messageIdInfo) ->
                                                Just messageIdInfo

                                            _ ->
                                                Nothing
                                        )
                                        blocknum
                                    )
                                    messagesForBlock
                                )
                        )
                    |> Dict.toList
                    |> List.map
                        (\( blocknum, messagesEl ) ->
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing 10
                                ]
                                [ Element.column
                                    [ Element.width Element.fill
                                    , Element.spacing 5
                                    , Element.Font.italic
                                    , Element.Font.size 14
                                    ]
                                    [ Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing 5
                                        ]
                                        [ Element.text <| "block " ++ String.fromInt blocknum
                                        , Element.el
                                            [ Element.width Element.fill
                                            , Element.height <| Element.px 1
                                            , Element.Border.color EH.black
                                            , Element.Border.widthEach
                                                { top = 1
                                                , bottom = 0
                                                , right = 0
                                                , left = 0
                                                }
                                            , Element.Border.dashed
                                            ]
                                            Element.none
                                        ]
                                    , blockTimes
                                        |> Dict.get blocknum
                                        |> Maybe.map posixToString
                                        |> Maybe.withDefault "???"
                                        |> Element.text
                                    ]
                                , messagesEl
                                ]
                        )

            miningMessagesList =
                if (List.length <| Dict.toList miningMessages) == 0 then
                    []

                else
                    [ Element.el
                        [ Element.Font.size 26 ]
                        (Element.text "Your mining messages")
                    , Element.column
                        [ Element.paddingXY 20 0 ]
                        (Dict.map
                            (viewMiningMessage
                                (case maybeShowAddressForPhace of
                                    Just (UserMiningMessage txHash) ->
                                        Just txHash

                                    _ ->
                                        Nothing
                                )
                            )
                            miningMessages
                            |> Dict.values
                        )
                    ]
        in
        Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            ]
        <|
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 20
                , Element.paddingEach
                    { bottom = 140
                    , top = 20
                    , right = 20
                    , left = 20
                    }
                ]
            <|
                messagesList
                    ++ miningMessagesList


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


viewMessage : Maybe ( Int, TxHash ) -> Int -> Message -> Element Msg
viewMessage maybeShowAddressIdInfo blocknum message =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.el
            [ Element.alignTop
            , Element.height <| Element.px 100
            ]
          <|
            phaceElement
                (MinedMessage ( blocknum, message.transactionHash ))
                message.from
                (maybeShowAddressIdInfo == Just ( blocknum, message.transactionHash ))
        , Element.column
            [ Element.width <| Element.px 100
            , Element.alignTop
            , Element.width Element.fill
            ]
            [ viewDaiBurned True message.burnAmount
            , viewMessageContent True message.message
            ]
        ]


viewMiningMessage : Maybe TxHash -> String -> MiningMessage -> Element Msg
viewMiningMessage maybeShowAddressForMessage txHashIdString miningMessage =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.el
            [ Element.alignTop
            , Element.height <| Element.px 100
            ]
          <|
            phaceElement
                (UserMiningMessage (Eth.Utils.unsafeToTxHash txHashIdString))
                miningMessage.draft.author
                (maybeShowAddressForMessage == Just (Eth.Utils.unsafeToTxHash txHashIdString))
        , Element.column
            [ Element.width <| Element.px 100
            , Element.alignTop
            , Element.width Element.fill
            ]
            [ Element.row
                [ Element.spacing 5 ]
                [ viewDaiBurned False miningMessage.draft.burnAmount
                , viewMiningMessageStatus txHashIdString miningMessage.status
                ]
            , viewMessageContent False miningMessage.draft.message
            ]
        ]


viewMiningMessageStatus : String -> MiningMessageStatus -> Element Msg
viewMiningMessageStatus txHashString status =
    Element.column
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.spacing 5
        , Element.Background.color <|
            case status of
                Failed _ ->
                    EH.softRed

                _ ->
                    Element.rgb 1 1 0
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignLeft
        ]
        [ Element.text (statusToString status)
        , Element.row
            [ Element.spacing 8 ]
            [ Element.text "Tx hash:"
            , Element.newTabLink
                [ Element.Font.color EH.blue ]
                { url = EthHelpers.etherscanTxUrl (Eth.Utils.unsafeToTxHash txHashString)
                , label = Element.text txHashString
                }
            ]
        ]


statusToString : MiningMessageStatus -> String
statusToString status =
    case status of
        Mining ->
            "Mining"

        Failed errStr ->
            "Error: " ++ errStr


viewDaiBurned : Bool -> TokenValue -> Element Msg
viewDaiBurned mined amount =
    Element.el
        [ Element.alignBottom
        , Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color <|
            if mined then
                EH.lightRed

            else
                Element.rgb 1 0.9 0.9
        , Element.Border.roundEach
            { bottomLeft = 0
            , bottomRight = 0
            , topLeft = 5
            , topRight = 5
            }
        , Element.alignLeft
        ]
    <|
        Element.row
            [ Element.spacing 3
            ]
            [ daiSymbol [ Element.height <| Element.px 18 ]
            , Element.text <| TokenValue.toConciseString amount
            ]


viewMessageContent : Bool -> String -> Element Msg
viewMessageContent mined content =
    renderMarkdownParagraphs
        ([ Element.spacing 2
         , Element.paddingXY 20 0
         , Element.Border.roundEach
            { topLeft = 0
            , topRight = 10
            , bottomRight = 10
            , bottomLeft = 10
            }
         , Element.alignTop
         ]
            ++ (if mined then
                    [ Element.Background.color (Element.rgb 0.8 0.8 1) ]

                else
                    [ Element.Background.color (Element.rgb 0.9 0.9 1)
                    , Element.Font.color EH.darkGray
                    ]
               )
        )
        content


renderMarkdownParagraphs : List (Attribute Msg) -> String -> Element Msg
renderMarkdownParagraphs attributes =
    Markdown.toHtml Nothing
        >> List.map Element.html
        >> Element.paragraph
            attributes


viewComposeUX : Maybe ( UserInfo, Bool ) -> ComposeUXModel -> Element Msg
viewComposeUX maybeUserInfoAndShowAddress composeUXModel =
    Element.column
        [ Element.width Element.fill
        , Element.Background.color EH.lightBlue
        , composeUXShadow
        , Element.Border.roundEach
            { topLeft = 0
            , topRight = 10
            , bottomRight = 10
            , bottomLeft = 10
            }
        , Element.above <|
            Element.el
                [ Element.alignLeft
                ]
            <|
                EH.blueButton
                    Mobile
                    []
                    [ "Hide" ]
                    (ShowComposeUX False)
        ]
        [ messageInputBox composeUXModel.message
        , actionFormAndMaybeErrorEl maybeUserInfoAndShowAddress composeUXModel
        ]


messageInputBox : String -> Element Msg
messageInputBox input =
    Element.el
        [ Element.width Element.fill
        , Element.padding 10
        , Element.Border.roundEach
            { bottomRight = 0
            , bottomLeft = 10
            , topRight = 10
            , topLeft = 10
            }
        , Element.Background.color EH.lightBlue
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


actionFormAndMaybeErrorEl : Maybe ( UserInfo, Bool ) -> ComposeUXModel -> Element Msg
actionFormAndMaybeErrorEl maybeUserInfoAndShowAddress composeUXModel =
    case maybeUserInfoAndShowAddress of
        Just ( userInfo, showAddress ) ->
            let
                ( goButtonEl, maybeErrorEls ) =
                    goButtonAndMaybeError userInfo composeUXModel
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
                    [ phaceElement User userInfo.address showAddress
                    , inputsElement userInfo composeUXModel
                    , goButtonEl
                    ]
                , inputErrorEl maybeErrorEls
                ]

        Nothing ->
            web3ConnectButton [ Element.centerX, Element.centerY ]


inputErrorEl : Maybe (List (Element Msg)) -> Element Msg
inputErrorEl =
    Maybe.map
        (Element.paragraph
            [ Element.width (Element.fill |> Element.maximum 700)
            , Element.Font.color EH.softRed
            , Element.Font.italic
            ]
        )
        >> Maybe.withDefault Element.none


inputsElement : UserInfo -> ComposeUXModel -> Element Msg
inputsElement userInfo composeUXModel =
    Element.el
        [ Element.centerY
        , Element.width <| Element.px 260
        ]
    <|
        Element.el [ Element.centerX ] <|
            case composeUXModel.miningUnlockTx of
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
                            [ Element.Font.color EH.blue
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
                                    , burnAmountInput composeUXModel.daiInput
                                    , Element.text "DAI"
                                    ]
                                , Element.row
                                    [ Element.Font.size 14
                                    , Element.spacing 5
                                    ]
                                    [ Element.Input.checkbox [ Element.alignTop ]
                                        { onChange = DonationCheckboxSet
                                        , icon = Element.Input.defaultCheckbox
                                        , checked = composeUXModel.donateChecked
                                        , label = Element.Input.labelHidden "Donate an extra 1% to Foundry"
                                        }
                                    , Element.column
                                        [ Element.spacing 5 ]
                                        [ Element.row []
                                            [ Element.text "Donate an extra 1% to "
                                            , Element.newTabLink
                                                [ Element.Font.color EH.blue ]
                                                { url = "https://foundrydao.com/"
                                                , label = Element.text "Foundry"
                                                }
                                            ]
                                        , Element.text "so we can build more cool stuff!"
                                        ]
                                    ]
                                ]


goButtonAndMaybeError : UserInfo -> ComposeUXModel -> ( Element Msg, Maybe (List (Element Msg)) )
goButtonAndMaybeError userInfo composeUXModel =
    case userInfo.balance of
        Just balance ->
            if TokenValue.isZero balance then
                ( maybeGoButton Nothing
                , Just
                    [ Element.text <|
                        "That account ("
                            ++ Eth.Utils.addressToChecksumString userInfo.address
                            ++ ") doesn't have any DAI! "
                    , Element.newTabLink [ Element.Font.color EH.blue ]
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
                                validateInputs composeUXModel
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
                                    ( maybeGoButton Nothing
                                    , Just
                                        [ Element.text "You don't have that much DAI in your wallet! "
                                        , Element.newTabLink [ Element.Font.color EH.blue ]
                                            { url = "https://kyberswap.com/swap/eth-dai"
                                            , label = Element.text "Kyberswap"
                                            }
                                        , Element.text " can swap your ETH for DAI in a single transaction."
                                        ]
                                    )

                                else
                                    case validateResults.message of
                                        Just message ->
                                            ( maybeGoButton <|
                                                Just <|
                                                    MessageDraft
                                                        userInfo.address
                                                        message
                                                        burnAmount
                                                        donateAmount
                                            , Nothing
                                            )

                                        Nothing ->
                                            ( maybeGoButton Nothing
                                            , Nothing
                                            )

                            Just (Err errStr) ->
                                ( maybeGoButton Nothing
                                , Just [ Element.text errStr ]
                                )

                            Nothing ->
                                ( maybeGoButton Nothing
                                , Nothing
                                )

                    _ ->
                        ( maybeGoButton Nothing
                        , Nothing
                        )

        _ ->
            ( maybeGoButton Nothing
            , Nothing
            )


maybeGoButton : Maybe MessageDraft -> Element Msg
maybeGoButton maybeDraft =
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
            EH.redButton
                Desktop
                commonStyles
                [ "Post" ]
                (Submit draft)

        Nothing ->
            EH.disabledButton
                Desktop
                commonStyles
                "Post"
                Nothing


loadingElement : List (Attribute Msg) -> Maybe String -> Element Msg
loadingElement attrs maybeString =
    Element.el
        ([ Element.Font.italic
         , Element.Font.color EH.darkGray
         , Element.Font.size 20
         ]
            ++ attrs
        )
        (Element.text <| Maybe.withDefault "loading..." maybeString)


web3ConnectButton : List (Attribute Msg) -> Element Msg
web3ConnectButton attrs =
    EH.redButton
        Mobile
        attrs
        [ "Connect to Wallet" ]
        ConnectToWeb3


unlockButton : List (Attribute Msg) -> Element Msg
unlockButton attrs =
    EH.redButton
        Desktop
        attrs
        [ "Unlock Dai" ]
        UnlockDai


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


showComposeUXButton : Element Msg
showComposeUXButton =
    EH.blueButton
        Desktop
        []
        [ "Compose Message" ]
        (ShowComposeUX True)


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
                    [ Element.Font.color EH.darkGray
                    , Element.Font.italic
                    ]
                    << List.map Element.text
                )
                [ [ "SmokeSignal messages are formatted with markdown (e.g. *italic*, **bold**, [link-title](url))." ]
                , [ "Hackmd.io is useful for drafting and previewing markdown text." ]
                ]


phaceElement : PhaceId -> Address -> Bool -> Element Msg
phaceElement phaceId fromAddress showAddress =
    let
        addressOutputEl =
            Element.el
                [ Element.alignBottom
                , Element.alignLeft
                , Element.Border.width 2
                , Element.Border.color EH.black
                , Element.Background.color EH.white
                , Element.Font.size 12
                , EH.moveToFront
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
            , Element.Border.width 2
            , Element.Border.color EH.black
            , Element.pointer
            , EH.onClickNoPropagation (ShowOrHideAddress phaceId)
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress)


userNoticeEls : DisplayProfile -> List (UserNotice Msg) -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft (20 |> changeForMobile 5 dProfile)
            , Element.moveUp (20 |> changeForMobile 5 dProfile)
            , Element.spacing (10 |> changeForMobile 5 dProfile)
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px (300 |> changeForMobile 150 dProfile)
            , Element.Font.size (15 |> changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight (20 |> changeForMobile 5 dProfile)
            , Element.moveDown 100
            , Element.spacing (10 |> changeForMobile 5 dProfile)
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px (300 |> changeForMobile 150 dProfile)
            , Element.Font.size (15 |> changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice : DisplayProfile -> ( Int, UserNotice Msg ) -> Element Msg
userNotice dProfile ( id, notice ) =
    let
        color =
            case notice.noticeType of
                UN.Update ->
                    Element.rgb255 100 200 255

                UN.Caution ->
                    Element.rgb255 255 188 0

                UN.Error ->
                    Element.rgb255 255 70 70

                UN.ShouldBeImpossible ->
                    Element.rgb255 200 200 200

        textColor =
            case notice.noticeType of
                UN.Error ->
                    Element.rgb 1 1 1

                _ ->
                    Element.rgb 0 0 0

        closeElement =
            Element.el
                [ Element.alignRight
                , Element.alignTop
                , Element.moveUp 5
                , Element.moveRight 5
                ]
                (EH.closeButton True (DismissNotice id))
    in
    Element.el
        [ Element.Background.color color
        , Element.Border.rounded (10 |> changeForMobile 5 dProfile)
        , Element.padding (8 |> changeForMobile 3 dProfile)
        , Element.width Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow
        , EH.onClickNoPropagation NoOp
        ]
        (notice.mainParagraphs
            |> List.indexedMap
                (\pNum paragraphLines ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Element.Font.color textColor
                        , Element.spacing 1
                        ]
                        (if pNum == 0 then
                            closeElement :: paragraphLines

                         else
                            paragraphLines
                        )
                )
            |> Element.column
                [ Element.spacing 4
                , Element.width Element.fill
                ]
        )


daiSymbol : List (Attribute Msg) -> Element Msg
daiSymbol attributes =
    Element.image attributes
        { src = "img/dai-unit-char.svg"
        , description = ""
        }
