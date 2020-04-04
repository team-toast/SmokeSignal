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
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Helpers.Element as EH
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
            [ Element.width Element.fill ]
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
        , viewMessages
            model.blockTimes
            model.messages
            (model.showAddress
                |> Maybe.andThen
                    (\phaceId ->
                        case phaceId of
                            MessageAuthor hash ->
                                Just hash

                            User ->
                                Nothing
                    )
            )
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
                    ++ [ Element.pointer
                       , Element.Events.onClick (ShowComposeUX True)
                       , Element.spacing 3
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


viewMessages : Dict Int Time.Posix -> Dict String Message -> Maybe Hex -> Element Msg
viewMessages blockTimes messages maybeShowAddressForMessage =
    let
        structuredMessageList =
            sortMessagesByBlock messages
                |> Dict.toList
    in
    if List.length structuredMessageList == 0 then
        Element.el
            [ Element.centerX
            , Element.centerY
            , Element.Font.size 36
            , Element.Font.color EH.darkGray
            , Element.Font.italic
            ]
            (Element.text "Searching for SmokeSignal messages...")

    else
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            , Element.spacing 20
            , Element.paddingEach
                { bottom = 140
                , top = 20
                , right = 20
                , left = 20
                }
            ]
        <|
            List.map
                (\( blocknum, messagesForBlock ) ->
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
                        , Element.column
                            [ Element.paddingXY 20 0 ]
                            (Dict.map
                                (viewMessage maybeShowAddressForMessage)
                                messagesForBlock
                                |> Dict.values
                            )
                        ]
                )
                structuredMessageList


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


sortMessagesByBlock : Dict String Message -> Dict Int (Dict String Message)
sortMessagesByBlock messages =
    messages
        |> Dict.toList
        |> Dict.Extra.groupBy
            (Tuple.second >> .block)
        |> Dict.map
            (always Dict.fromList)


viewMessage : Maybe Hex -> String -> Message -> Element Msg
viewMessage maybeShowAddressForMessage hashIdString message =
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
                (MessageAuthor (Eth.Utils.unsafeToHex hashIdString))
                message.author
                (maybeShowAddressForMessage == Just (Eth.Utils.unsafeToHex hashIdString))
        , Element.column
            [ Element.width <| Element.px 100
            , Element.alignTop
            , Element.width Element.fill
            ]
            [ viewDaiBurned message.burnAmount
            , viewMessageContent message.message
            ]
        ]


viewDaiBurned : TokenValue -> Element Msg
viewDaiBurned amount =
    Element.el
        [ Element.Font.size 22
        , Element.paddingXY 10 5
        , Element.Background.color EH.lightRed
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


viewMessageContent : String -> Element Msg
viewMessageContent content =
    renderMarkdownParagraphs
        [ Element.spacing 2
        , Element.paddingXY 20 0
        , Element.Border.roundEach
            { topLeft = 0
            , topRight = 10
            , bottomRight = 10
            , bottomLeft = 10
            }
        , Element.Background.color (Element.rgb 0.8 0.8 1)
        , Element.alignTop
        ]
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

                -- , Element.Background.color EH.lightBlue
                -- , composeUXShadow
                -- , Element.paddingEach
                --     { bottom = 0
                --     , top = 10
                --     , right = 10
                --     , left = 10
                --     }
                -- , Element.Border.roundEach
                --     { topRight = 10
                --     , topLeft = 10
                --     , bottomRight = 0
                --     , bottomLeft = 0
                --     }
                ]
            <|
                EH.blueButton
                    Mobile
                    []
                    [ "Hide" ]
                    (ShowComposeUX False)
        ]
        [ messageInputBox composeUXModel.message
        , actionFormAndMaybeError maybeUserInfoAndShowAddress composeUXModel
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


actionFormAndMaybeError : Maybe ( UserInfo, Bool ) -> ComposeUXModel -> Element Msg
actionFormAndMaybeError maybeUserInfoAndShowAddress composeUXModel =
    let
        ( actionFormEl, maybeInputErrorStr ) =
            actionFormElAndMaybeError maybeUserInfoAndShowAddress composeUXModel
    in
    Element.row
        [ Element.alignLeft
        , Element.spacing 10
        ]
        [ actionFormEl
        , maybeInputErrorStr
            |> Maybe.map inputErrorEl
            |> Maybe.withDefault Element.none
        ]


actionFormElAndMaybeError : Maybe ( UserInfo, Bool ) -> ComposeUXModel -> ( Element Msg, Maybe String )
actionFormElAndMaybeError maybeUserInfoAndShowAddress composeUXModel =
    case maybeUserInfoAndShowAddress of
        Just ( userInfo, showAddress ) ->
            let
                ( goButtonEl, maybeError ) =
                    goButtonAndMaybeError userInfo composeUXModel
            in
            ( Element.row
                [ Element.spacing 15
                , Element.padding 10
                , Element.Background.color <| Element.rgb 0.8 0.8 1
                , Element.Border.rounded 10
                ]
                [ phaceElement User userInfo.address showAddress
                , inputsElement userInfo composeUXModel
                , goButtonEl
                ]
            , maybeError
            )

        Nothing ->
            ( web3ConnectButton [ Element.centerX, Element.centerY ]
            , Nothing
            )


inputErrorEl : String -> Element Msg
inputErrorEl error =
    Element.paragraph
        [ Element.width <| Element.px 300
        , Element.Font.color EH.softRed
        , Element.Font.italic
        ]
        [ Element.text error ]


inputsElement : UserInfo -> ComposeUXModel -> Element Msg
inputsElement userInfo composeUXModel =
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


goButtonAndMaybeError : UserInfo -> ComposeUXModel -> ( Element Msg, Maybe String )
goButtonAndMaybeError userInfo composeUXModel =
    case ( userInfo.balance, userInfo.daiUnlocked ) of
        ( Just balance, Just True ) ->
            case validateInputs composeUXModel of
                Just (Ok validatedInputs) ->
                    let
                        balanceTooLow =
                            TokenValue.compare validatedInputs.burnAmount balance == GT
                    in
                    if balanceTooLow then
                        ( maybeGoButton Nothing
                        , Just "You don't have that much DAI in your wallet!"
                        )

                    else
                        ( maybeGoButton <| Just validatedInputs
                        , Nothing
                        )

                Just (Err errStr) ->
                    ( maybeGoButton Nothing
                    , Just errStr
                    )

                Nothing ->
                    ( maybeGoButton Nothing
                    , Nothing
                    )

        _ ->
            ( maybeGoButton Nothing
            , Nothing
            )


maybeGoButton : Maybe ValidatedInputs -> Element Msg
maybeGoButton maybeValidInputs =
    let
        commonStyles =
            [ Element.height <| Element.px 100
            , Element.width <| Element.px 100
            , Element.Font.size 26
            , Element.Border.rounded 10
            ]
    in
    case maybeValidInputs of
        Just validInputs ->
            EH.redButton
                Desktop
                commonStyles
                [ "Post" ]
                (Submit validInputs)

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
        addressOutputEl isInFront =
            Element.el
                [ Element.alignTop
                , Element.alignLeft
                , Element.Border.widthEach
                    { top = 2
                    , bottom =
                        if isInFront then
                            1

                        else
                            2
                    , right = 2
                    , left = 2
                    }
                , Element.Border.color EH.black
                , Element.Background.color EH.white
                , Element.Font.size 12
                ]
                (Element.text <| Eth.Utils.addressToChecksumString fromAddress)
    in
    Element.el
        (if showAddress then
            [ Element.inFront (addressOutputEl True)
            , Element.behindContent (addressOutputEl False)
            , EH.moveToFront
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
            , Element.Events.onMouseEnter (ShowAddress phaceId)
            , Element.Events.onMouseLeave HideAddress
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
