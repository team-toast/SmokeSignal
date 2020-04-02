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
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Html.Attributes
import Markdown
import Phace
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
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
        [ Element.width Element.fill
        , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
        , Element.padding 20
        , Element.spacing 30
        , Element.inFront <|
            if model.showComposeUX then
                Element.none

            else
                viewMinimizedComposeUX
                    (Wallet.userInfo model.wallet)
                    model.showingAddress
        ]
        [ title
        , viewMessages model.blockTimes model.messages model.showingAddress
        , if model.showComposeUX then
            Element.el
                [ Element.width Element.fill
                , Element.alignBottom
                ]
                (viewComposeUX (Wallet.userInfo model.wallet) model.showingAddress model.composeUXModel)

          else
            Element.none
        ]


viewMinimizedComposeUX : Maybe UserInfo -> Maybe Address -> Element Msg
viewMinimizedComposeUX maybeUserInfo showingAddress =
    Element.el
        [ Element.alignRight
        , Element.alignBottom
        , Element.Background.color EH.lightGray
        , Element.padding 10
        , Element.Border.roundEach
            { topLeft = 10
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 0
            }
        , Element.Border.widthEach
            { top = 1
            , left = 1
            , right = 0
            , bottom = 0
            }
        ]
    <|
        case maybeUserInfo of
            Nothing ->
                web3ConnectButton []

            Just accountInfo ->
                Element.column
                    [ Element.spacing 10 ]
                    [ phaceElement accountInfo.address (showingAddress == Just accountInfo.address)
                    , showComposeUXButton
                    ]


title : Element Msg
title =
    Element.el
        [ Element.Font.size 40
        , Element.Font.bold
        ]
    <|
        Element.text "SmokeSignal"


viewMessages : Dict Int Time.Posix -> List Message -> Maybe Address -> Element Msg
viewMessages blockTimes messages showingAddress =
    let
        structuredMessageList =
            sortMessagesByBlock messages
                |> Dict.toList
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Element.spacing 20
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
                        (List.map (viewMessage showingAddress) messagesForBlock)
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


sortMessagesByBlock : List Message -> Dict Int (List Message)
sortMessagesByBlock messages =
    messages
        |> Dict.Extra.groupBy
            .block


viewMessage : Maybe Address -> Message -> Element Msg
viewMessage showingAddress message =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.el
            [ Element.alignTop
            , Element.height <| Element.px 100
            ]
          <|
            phaceElement message.from (showingAddress == Just message.from)
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
        [ Element.Font.size 26
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


viewComposeUX : Maybe UserInfo -> Maybe Address -> ComposeUXModel -> Element Msg
viewComposeUX maybeUserInfo showingAddress composeUXModel =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ messageInputBox composeUXModel.message
        , actionFormAndMaybeError maybeUserInfo showingAddress composeUXModel
        ]


messageInputBox : String -> Element Msg
messageInputBox input =
    Element.Input.multiline
        [ Element.width Element.fill
        , Element.height (Element.px 300)
        ]
        { onChange = MessageInputChanged
        , text = input
        , placeholder = Just messageInputPlaceholder
        , label = Element.Input.labelHidden "messageInput"
        , spellcheck = True
        }


actionFormAndMaybeError : Maybe UserInfo -> Maybe Address -> ComposeUXModel -> Element Msg
actionFormAndMaybeError maybeUserInfo showingAddress composeUXModel =
    let
        ( actionFormEl, maybeInputErrorStr ) =
            actionFormElAndMaybeError maybeUserInfo showingAddress composeUXModel
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


actionFormElAndMaybeError : Maybe UserInfo -> Maybe Address -> ComposeUXModel -> ( Element Msg, Maybe String )
actionFormElAndMaybeError maybeUserInfo showingAddress composeUXModel =
    case maybeUserInfo of
        Just userInfo ->
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
                [ phaceElement userInfo.address (showingAddress == Just userInfo.address)
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
        Desktop
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


phaceElement : Address -> Bool -> Element Msg
phaceElement fromAddress showAddress =
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
            , Element.Events.onMouseEnter (ShowAddress fromAddress)
            , Element.Events.onMouseLeave HideAddress
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress)


daiSymbol : List (Attribute Msg) -> Element Msg
daiSymbol attributes =
    Element.image attributes
        { src = "img/dai-unit-char.svg"
        , description = ""
        }
