module View.Compose exposing (view)

import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), black, responsiveVal, white)
import Maybe.Extra exposing (unwrap)
import Result.Extra
import Theme exposing (orange, theme)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, sansSerifFont, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (appStatusMessage, daiAmountInput, phaceElement, shortenedHash, viewContext, web3ConnectButton, whenAttr, whenJust, wrapModal)
import View.Img
import View.Markdown
import Wallet


view : Model -> Element Msg
view model =
    model.wallet
        |> Wallet.userInfo
        |> unwrap
            ([ text "Please connect your Metamask wallet." ]
                |> Element.paragraph [ Font.center, centerY ]
                |> el
                    [ padding 10
                    , whiteGlowAttributeSmall
                    , Background.color black
                    , Font.color white
                    , height <| px 250
                    , width fill
                    ]
            )
            (viewBox model)
        |> wrapModal ComposeClose


viewBox : Model -> UserInfo -> Element Msg
viewBox model userInfo =
    let
        submitEnabled =
            not (String.isEmpty model.compose.body)
                && not (String.isEmpty model.compose.dollar)
    in
    [ "Comment"
        |> text
        |> el [ sansSerifFont, Font.color white, centerX ]
        |> el
            [ View.Attrs.sansSerifFont
            , padding 10
            , slightRound
            , Background.color Theme.orange
            , Font.bold
            , Font.color white
            , Font.size 20
            , width fill
            ]
    , [ [ [ Input.text
                [ width fill
                , View.Attrs.whiteGlowAttributeSmall
                ]
                { onChange = ComposeTitleChange
                , label = Input.labelHidden ""
                , placeholder =
                    "Post Title"
                        |> text
                        |> Input.placeholder []
                        |> Just
                , text = model.compose.title
                }
          , case model.compose.context of
                TopLevel topic ->
                    "#"
                        ++ topic
                        |> text
                        |> el [ Font.color orange, centerY ]
                        |> el [ height fill, Font.size 25, Font.bold ]

                Reply _ ->
                    View.Img.replyArrow 25 orange
                        |> el [ centerY ]
          ]
            |> column [ width fill, height fill ]
        , [ text "🔥"
                |> el [ Font.size 30 ]
          , text "BURN"
                |> el [ Font.size 15, centerX ]
          ]
            |> column
                [ spacing 10
                , Font.color orange
                , Font.bold
                ]
        , [ [ View.Img.dollar 30 white
            , Input.text
                [ View.Attrs.whiteGlowAttributeSmall
                , Background.color white
                , width <| px 250
                ]
                { onChange = ComposeDollarChange
                , label = Input.labelHidden ""
                , placeholder =
                    "00.00"
                        |> text
                        |> Input.placeholder []
                        |> Just
                , text = model.compose.dollar
                }
            ]
                |> row [ spacing 5 ]
          , [ Input.checkbox
                [ width <| px 30
                , height <| px 30
                , Background.color white
                , whiteGlowAttributeSmall
                , hover
                ]
                { onChange = Types.DonationCheckboxSet
                , icon =
                    \checked ->
                        "✔️"
                            |> text
                            |> el
                                [ centerX
                                , centerY
                                , Font.size 25
                                ]
                            |> View.Common.when checked
                , checked = model.compose.donate
                , label = Input.labelHidden "Donate an extra 1% to Foundry"
                }
            , [ text "Donate an extra 1% to "
              , Element.newTabLink
                    [ Font.color theme.linkTextColor, hover ]
                    { url = "https://foundrydao.com/"
                    , label = text "Foundry"
                    }
              , text " so we can build more cool stuff!"
              ]
                |> Element.paragraph [ spacing 5, Font.color white ]
            ]
                |> row
                    [ Font.size (responsiveVal model.dProfile 14 10)
                    , spacing 10
                    ]
          ]
            |> column [ height fill, spacing 10 ]
        , [ text "💎"
                |> el [ Font.size 30 ]
          , text "ETH"
                |> el [ Font.size 15, centerX ]
          ]
            |> column
                [ spacing 10
                , Font.color orange
                , Font.bold
                ]
        , phaceElement
            ( 75, 75 )
            False
            userInfo.address
            False
            ClickHappened
            |> el [ centerY ]
        ]
            |> row [ width fill, spacing 20, sansSerifFont ]
      , [ [ Input.multiline
                [ width fill
                , height fill
                , View.Attrs.whiteGlowAttributeSmall
                , Background.color black
                , Font.color white
                , Input.button
                    [ padding 10
                    , Background.color black
                    , Element.alignRight
                    , View.Attrs.roundBorder
                    , hover
                        |> whenAttr submitEnabled
                    ]
                    { onPress =
                        if submitEnabled then
                            Just SubmitDraft

                        else
                            Nothing
                    , label = text "Comment"
                    }
                    |> el
                        [ width fill
                        , padding 10
                        , Background.color orange
                        , Element.alignBottom
                        ]
                    |> Element.inFront
                ]
                { onChange = Types.ComposeBodyChange
                , label = Input.labelHidden ""
                , placeholder =
                    "What do you want to say?"
                        |> text
                        |> Input.placeholder []
                        |> Just
                , text = model.compose.body
                , spellcheck = False
                }
          ]
            |> column
                [ width fill
                , height fill
                , spacing 20
                ]
        , model.compose.body
            |> View.Markdown.renderString
                [ height fill
                , width fill
                , Element.clip
                , whiteGlowAttributeSmall
                , Font.color white
                , padding 10
                ]
            |> Result.Extra.unpack
                (\err ->
                    [ text err ]
                        |> Element.paragraph [ whiteGlowAttributeSmall ]
                )
                identity
        ]
            |> row
                [ height fill
                , width fill
                , spacing 30
                , sansSerifFont
                ]
      ]
        |> column
            [ height fill
            , width fill
            , spacing 20
            , padding 30
            ]
    ]
        |> column
            [ width <| px 1000
            , height <| px 700
            , Background.color black
            , whiteGlowAttributeSmall
            ]


viewBody : EH.DisplayProfile -> Bool -> Wallet -> Maybe PhaceIconId -> Model -> Element Msg
viewBody dProfile donateChecked wallet showAddressId model =
    let
        commonAttributes =
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color theme.postBodyBackground
            , composeUXShadow
            , Border.rounded 10
            , Element.inFront <|
                EH.closeButton
                    [ Element.alignTop
                    , Element.alignRight
                    ]
                    (Element.rgb 0.3 0.3 0.3)
                    ComposeOpen
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
                    --[ viewInput dProfile model.content
                    [ Element.el [ Element.alignRight ] <| actionFormAndMaybeErrorEl dProfile donateChecked wallet showAddressId model
                    ]

                --, viewPreviewWithPostContext dProfile Nothing model.renderedPreview model.context
                ]

        Mobile ->
            Element.column
                (commonAttributes
                    ++ [ Element.padding 10
                       , Element.spacing 10
                       , Font.size 20
                       ]
                )
                --(if model.showPreviewOnMobile then
                (if True then
                    --[ viewPreviewWithPostContext
                    --dProfile
                    --(case Wallet.userInfo wallet of
                    --Just userInfo ->
                    --Just <|
                    --( userInfo.address
                    --, showAddressId == Just PhaceForPreview
                    --)
                    --Nothing ->
                    --Nothing
                    --)
                    --model.renderedPreview
                    --model.context
                    [ actionFormAndMaybeErrorEl dProfile donateChecked wallet showAddressId model
                    ]

                 else
                    --[ viewInput dProfile model.content
                    [ viewPreviewButton
                        dProfile
                        --(model.content.body /= "")
                        False
                    ]
                )


validateInputs : Bool -> Model -> CheckedMaybeValidInputs
validateInputs donateChecked composeModel =
    { content =
        --if composeModel.content.body == "" then
        if True then
            Nothing

        else
            --Just composeModel.content
            Nothing
    , burnAndDonateAmount =
        validateBurnAmount ""
            --validateBurnAmount composeModel.daiInput
            |> Maybe.map
                (Result.map
                    (\burnAmount ->
                        ( burnAmount
                        , if donateChecked then
                            TokenValue.div burnAmount 100

                          else
                            TokenValue.zero
                        )
                    )
                )
    }


validateBurnAmount : String -> Maybe (Result String TokenValue)
validateBurnAmount input =
    if input == "" then
        Nothing

    else
        Just
            (TokenValue.fromString input
                |> Result.fromMaybe "Invalid burn amount"
                |> Result.andThen
                    (\tv ->
                        if TokenValue.compare tv TokenValue.zero == GT then
                            Ok tv

                        else
                            Err "Minimum amount is 0.000000000000000001 DAI"
                    )
            )


composeUXShadow : Attribute Msg
composeUXShadow =
    Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = Element.rgba 0 0 0 1
        }


viewPreviewButton : DisplayProfile -> Bool -> Element Msg
viewPreviewButton dProfile enabled =
    if enabled then
        theme.secondaryActionButton
            dProfile
            []
            [ "Preview" ]
            (EH.Action ClickHappened)
        --MobilePreviewToggle)

    else
        theme.disabledActionButton
            dProfile
            []
            "Preview"


viewInput : DisplayProfile -> Content -> Element Msg
viewInput dProfile content =
    EH.scrollbarYEl [] <|
        Input.multiline
            ([ Element.width Element.fill
             , Element.height Element.fill
             , Element.padding (responsiveVal dProfile 10 5)
             , Background.color <| Element.rgba 1 1 1 0.5
             ]
                ++ responsiveVal dProfile
                    []
                    [ Font.size 18 ]
            )
            { onChange = always ClickHappened -- BodyInputChanged
            , text = content.body
            , placeholder = Just messageInputPlaceholder
            , label = Input.labelHidden "messageInput"
            , spellcheck = True
            }


viewPreviewWithPostContext : DisplayProfile -> Maybe ( Address, Bool ) -> Maybe (Element Never) -> Context -> Element Msg
viewPreviewWithPostContext dProfile maybeShowPhaceInfo renderedContent context =
    EH.scrollbarYEl [] <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 15
            , Background.color <| Element.rgba 1 1 1 0.5
            , Border.width 1
            , Border.color <| Element.rgba 0 0 0 0.5
            , Border.rounded 10
            , Element.spacing 15
            ]
            [ Element.row
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                [ case maybeShowPhaceInfo of
                    Just ( fromAddress, showAddress ) ->
                        --phaceElement
                        --( 100, 100 )
                        --True
                        --fromAddress
                        --showAddress
                        --(MsgUp <| ShowOrHideAddress PhaceForPreview)
                        --(MsgUp NoOp)
                        Element.none

                    Nothing ->
                        Element.none
                , Element.el [ Element.alignLeft ] <|
                    viewContext context
                ]
            , case renderedContent of
                Nothing ->
                    appStatusMessage theme.appStatusTextColor "[Preview Box]"

                Just rendered ->
                    Element.map never rendered
            ]


messageInputPlaceholder : Input.Placeholder Msg
messageInputPlaceholder =
    Input.placeholder [] <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
        <|
            List.map
                (Element.paragraph
                    [ Font.color theme.messageInputPlaceholderTextColor
                    , Font.italic
                    ]
                    << List.map Element.text
                )
                [ [ "SmokeSignal messages are formatted with markdown (e.g. *italic*, **bold**, [link-title](url))." ]
                , [ "Hackmd.io is useful for drafting and previewing markdown text." ]
                ]


viewReplyInfo : PostId -> Element Msg
viewReplyInfo postId =
    Element.column
        [ Element.padding 10
        , Border.rounded 5
        , Font.size 20
        , Font.italic
        , Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 3
        ]
        [ Element.text "Replying to:"
        , Element.el
            [ Font.color theme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                GotoView <|
                    ViewPost postId

            --ViewPost postId
            ]
            (Element.text <|
                shortenedHash postId.messageHash
            )
        ]


viewTopic : String -> Element Msg
viewTopic topic =
    Element.column
        [ Element.padding 10
        , Border.rounded 5
        , Font.size 20
        , Font.italic
        , Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        , Element.scrollbarX
        , Element.clipX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "Topic:"
        , Element.el
            [ Font.color theme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                GotoView <|
                    ViewTopic topic

            --Topic topic
            ]
            (Element.text <| topic)
        ]


actionFormAndMaybeErrorEl : DisplayProfile -> Bool -> Wallet -> Maybe PhaceIconId -> Model -> Element Msg
actionFormAndMaybeErrorEl dProfile donateChecked wallet showAddressId model =
    case Wallet.userInfo wallet of
        Just userInfo ->
            let
                ( goButtonEl, maybeErrorEls ) =
                    goButtonAndMaybeError dProfile donateChecked userInfo model

                actionRow =
                    Element.row
                        [ Element.spacing 15
                        , Element.padding 10
                        , Background.color <| Element.rgb 0.8 0.8 1
                        , Border.rounded 10
                        ]
                        [ case dProfile of
                            Desktop ->
                                --phaceElement
                                --( 100, 100 )
                                --True
                                --userInfo.address
                                --(showAddressId == Just UserPhace)
                                --(Types.ShowOrHideAddress UserPhace)
                                --NoOp
                                goBackButton dProfile

                            Mobile ->
                                goBackButton dProfile
                        , inputsElement dProfile donateChecked userInfo
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

        Nothing ->
            web3ConnectButton
                dProfile
                [ Element.centerX, Element.centerY ]


inputsElement : EH.DisplayProfile -> Bool -> UserInfo -> Element Msg
inputsElement dProfile donateChecked userInfo =
    Element.el
        ([ Element.centerY
         , Element.centerX
         , Font.size (responsiveVal dProfile 20 14)
         ]
            ++ (case dProfile of
                    Desktop ->
                        [ Element.width <| Element.px 260 ]

                    Mobile ->
                        []
               )
        )
    <|
        Element.el [] <|
            Element.column
                [ Element.spacing 10 ]
                [ Element.row
                    [ Element.spacing (responsiveVal dProfile 10 5)
                    , Element.centerX
                    ]
                    [ Element.text "Burn"
                    , daiAmountInput
                        dProfile
                        ""
                        --model.daiInput
                        --DaiInputChanged
                        (always ClickHappened)
                    , Element.text "DAI"
                    ]
                , Element.row
                    [ Font.size (responsiveVal dProfile 14 10)
                    , Element.spacing 5
                    ]
                    [ Input.checkbox [ Element.alignTop ]
                        { onChange = Types.DonationCheckboxSet
                        , icon = Input.defaultCheckbox
                        , checked = donateChecked
                        , label = Input.labelHidden "Donate an extra 1% to Foundry"
                        }
                    , Element.column
                        [ Element.spacing 5 ]
                        [ Element.row []
                            [ Element.text "Donate an extra 1% to "
                            , Element.newTabLink
                                [ Font.color theme.linkTextColor ]
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
            [ Font.color theme.errorTextColor
            , Font.italic
            ]
    in
    case dProfile of
        Desktop ->
            Element.paragraph
                (commonAttributes
                    ++ [ Element.width (Element.fill |> Element.maximum 200)
                       , Element.alignTop
                       , Font.alignRight
                       ]
                )
                (els |> Maybe.withDefault [ Element.text " " ])

        Mobile ->
            Element.paragraph
                (commonAttributes
                    ++ [ Element.width Element.fill
                       , Font.size 14
                       ]
                )
                (els |> Maybe.withDefault [ Element.text " " ])


goButtonAndMaybeError : EH.DisplayProfile -> Bool -> UserInfo -> Model -> ( Element Msg, Maybe (List (Element Msg)) )
goButtonAndMaybeError dProfile donateChecked userInfo model =
    case userInfo.balance of
        Just balance ->
            if TokenValue.isZero balance then
                ( maybeGoButton dProfile Nothing
                , Just
                    [ Element.text <|
                        "That account ("
                            ++ Eth.Utils.addressToChecksumString userInfo.address
                            ++ ") doesn't have any DAI! "
                    , Element.newTabLink [ Font.color theme.linkTextColor ]
                        { url = "https://kyberswap.com/swap/eth-dai"
                        , label = Element.text "Kyberswap"
                        }
                    , Element.text " can swap your ETH for DAI in a single transaction."
                    ]
                )

            else
                case 1 of
                    1 ->
                        let
                            validateResults =
                                validateInputs donateChecked model
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
                                        , Element.newTabLink [ Font.color theme.linkTextColor ]
                                            { url = "https://kyberswap.com/swap/eth-dai"
                                            , label = Element.text "Kyberswap"
                                            }
                                        , Element.text " can swap your ETH for DAI in a single transaction."
                                        ]
                                    )

                                else
                                    let
                                        maybeUpToDateRender =
                                            --if model.renderNeeded then
                                            if True then
                                                Nothing

                                            else
                                                --model.renderedPreview
                                                Nothing
                                    in
                                    case ( validateResults.content, maybeUpToDateRender ) of
                                        ( Just content, Just rendered ) ->
                                            ( maybeGoButton dProfile <|
                                                --Just <|
                                                --Draft
                                                --donateAmount
                                                --(Core
                                                --userInfo.address
                                                --burnAmount
                                                --content
                                                --(Post.buildMetadataFromContext model.context)
                                                --rendered
                                                --)
                                                Nothing
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
    [ Element.height <| Element.px (responsiveVal dProfile 100 70)
    , Element.width <| Element.px (responsiveVal dProfile 100 70)
    , Font.size (responsiveVal dProfile 26 20)
    , Border.rounded (responsiveVal dProfile 10 7)
    ]


maybeGoButton : EH.DisplayProfile -> Maybe Draft -> Element Msg
maybeGoButton dProfile maybeDraft =
    case maybeDraft of
        Just draft ->
            theme.emphasizedActionButton
                dProfile
                (commonActionButtonStyles dProfile)
                [ "GO" ]
                (EH.Action <| SubmitPost draft)

        Nothing ->
            theme.disabledActionButton
                dProfile
                (commonActionButtonStyles dProfile)
                "GO"


goBackButton : EH.DisplayProfile -> Element Msg
goBackButton dProfile =
    theme.secondaryActionButton
        dProfile
        (commonActionButtonStyles dProfile)
        [ "Edit" ]
        -- MobilePreviewToggle)
        (EH.Action <| ClickHappened)
