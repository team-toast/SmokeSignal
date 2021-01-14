module Home.View exposing (banner, view)

import Common.Types exposing (Context(..), Id, MsgUp(..), PhaceIconId(..), Post(..), Published, PublishedPostsDict, Route(..), ViewContext(..))
import Common.View exposing (daiSymbol, phaceElement, whiteGlowAttribute, whiteGlowAttributeSmall)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element, column, el, fill, fillPortion, padding, paddingXY, row, text, width)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..))
import Helpers.Time as TimeHelpers
import Home.Types exposing (Model, Msg(..))
import Html.Attributes
import Maybe.Extra
import Misc exposing (getPublishedPostFromId)
import Post
import PostUX.Preview as PostPreview
import PostUX.Types as PostUX
import Routing
import Theme exposing (almostWhite, theme)
import Time
import TokenValue exposing (TokenValue)
import Wallet


view :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PhaceIconId
    -> String
    -> Common.Types.Wallet
    -> PublishedPostsDict
    -> Model
    -> Element Msg
view dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model =
    case dProfile of
        Desktop ->
            column
                ([ Element.centerX
                 , width <| Element.px 1000
                 , Element.spacing majorSpacing
                 ]
                    ++ (List.map Element.inFront <|
                            viewModals
                                dProfile
                                model.showNewToSmokeSignalModal
                       )
                )
                [ el
                    [ width fill
                    , paddingXY 10 0
                    ]
                  <|
                    banner
                        dProfile
                , body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model
                ]

        Mobile ->
            text "mobile view"


viewModals :
    DisplayProfile
    -> Bool
    -> List (Element Msg)
viewModals dProfile showNewToSmokeSignalModal =
    Maybe.Extra.values
        [ if showNewToSmokeSignalModal == True then
            Just <|
                EH.modal
                    (Element.rgba 0 0 0 0.25)
                    False
                    CloseNewToSmokeSignalModal
                    CloseNewToSmokeSignalModal
                <|
                    viewNewToSmokeSignalModal dProfile

          else
            Nothing
        ]


viewNewToSmokeSignalModal :
    DisplayProfile
    -> Element Msg
viewNewToSmokeSignalModal dProfile =
    column
        [ whiteGlowAttribute
        , Element.Border.rounded 10
        , Element.Font.color EH.white
        , width fill
        , Element.Background.color <| Element.rgba 0 0 0 0.85
        , padding 50
        , Element.spacing 30
        ]
        [ Element.text "Welcome to" |> rowElement dProfile []
        , rowElement
            dProfile
            []
          <|
            Element.image
                [ width <| Element.px 200
                ]
                { src = "img/smokesignal-logo-vertical.svg"
                , description =
                    "smokesignal logo"
                }
        , rowElement
            dProfile
            []
          <|
            Element.text "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat."
        , rowElement
            dProfile
            []
          <|
            el
                [ Element.Font.color Theme.orange, Element.Font.size 28, Element.Font.bold ]
            <|
                Element.text "No Usernames. No Moderators. No censorship. No Deplatforming."
        , rowElement
            dProfile
            []
          <|
            Element.text "All you need is ETH for gas and DAI to burn."
        , rowElement
            dProfile
            []
          <|
            column
                [ Element.spacing 5 ]
                [ rowElement dProfile [] <| Element.text "All SmokeSignal posts are permanent and impossible to delete, and can be"
                , rowElement dProfile [] <| Element.text "accessed with any browser via an IPFS Gateway (example)"
                , rowElement dProfile [] <| Element.text "or the smokesignal.eth.link mirror (example)."
                ]
        , rowElement
            dProfile
            []
          <|
            column
                [ Element.spacing 5 ]
                [ rowElement dProfile [] <| Element.text "If the above two methods prove unreliable, some browsers also support direct"
                , rowElement dProfile [] <| Element.text "smokesignal.eth links (example) or direct IPFS links (example)."
                ]
        , rowElement
            dProfile
            []
          <|
            el
                [ Element.Font.color Theme.orange
                , Element.Font.semiBold
                ]
            <|
                Element.text
                    "Go to introductory video →"
        ]


rowElement :
    DisplayProfile
    -> List (Attribute Msg)
    -> Element Msg
    -> Element Msg
rowElement dProfile attributes element =
    row
        ([ Element.height fill
         , Element.centerX
         ]
            ++ attributes
        )
        [ element ]


banner :
    DisplayProfile
    -> Element Msg
banner dProfile =
    el
        [ Element.centerX
        , Element.Font.color EH.white
        ]
        (Element.text "banner here")
        |> el
            [ width fill
            , Element.height <| Element.px 200
            , Element.Background.color EH.black
            , whiteGlowAttribute
            ]


body :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe PhaceIconId
    -> String
    -> Common.Types.Wallet
    -> PublishedPostsDict
    -> Model
    -> Element Msg
body dProfile donateChecked blockTimes now showAddressId demoPhaceSrc wallet posts model =
    let
        xs =
            Dict.values posts
                |> List.concat
                |> List.take 3

        maybeShowAddressForPostId =
            case showAddressId of
                Just (PhaceForPublishedPost id) ->
                    Just id

                _ ->
                    Nothing
    in
    [ column
        [ Element.spacing majorSpacing
        , Element.alignTop
        , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , width <| fillPortion 2
        , padding 10
        ]
        [ orangeBannerEl
            dProfile
            [ Element.pointer
            , Element.Events.onClick ShowNewToSmokeSignalModal
            ]
            40
            20
            "NEW TO SMOKESIGNAL?"
        , column
            [ width fill
            , Element.spacing 3
            ]
            [ orangeBannerEl
                dProfile
                []
                20
                10
                "RECENT POSTS..."
            , postFeed dProfile donateChecked blockTimes now maybeShowAddressForPostId wallet xs
            ]
        ]
    , column
        [ width <| fillPortion 1
        , Element.alignTop
        , padding 10
        , Element.spacing 20
        ]
        [ walletUXPane dProfile showAddressId demoPhaceSrc wallet
        , topicsUX dProfile model.topicSearchInput posts
        ]
    ]
        |> row
            [ fill
                |> Element.maximum 1000
                |> width
            , Element.spacing majorSpacing
            ]


orangeBannerEl :
    DisplayProfile
    -> List (Attribute Msg)
    -> Int
    -> Int
    -> String
    -> Element Msg
orangeBannerEl dProfile attributes fontSize paddingVal bannerText =
    el
        ([ width fill
         , padding paddingVal
         , Element.Font.size fontSize
         , Element.Background.color Theme.orange
         , Element.Font.semiBold
         , Element.Font.color EH.white
         , whiteGlowAttribute
         , Element.Border.rounded 10
         ]
            ++ attributes
        )
    <|
        Element.text bannerText


topicsUX :
    DisplayProfile
    -> String
    -> PublishedPostsDict
    -> Element Msg
topicsUX dProfile topicsSearchInput posts =
    column
        [ Element.spacing 10
        , Element.centerX
        , width (fill |> Element.minimum 400)
        ]
        [ orangeBannerEl
            dProfile
            []
            26
            12
            "TOPICS"
        , column
            [ width fill
            , Element.alignTop
            , Element.spacing 1
            ]
            [ Element.Input.text
                [ width fill
                , Element.Background.color EH.black
                , Element.Border.color Theme.almostWhite
                , whiteGlowAttributeSmall
                , Element.Font.color EH.white
                ]
                { onChange = SearchInputChanged
                , text = topicsSearchInput
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.color EH.white
                            , Element.Font.italic
                            ]
                            (Element.text "Find or Create Topic...")
                , label = Element.Input.labelHidden "topic"
                }
            , topicsColumn
                dProfile
                (Post.sanitizeTopic topicsSearchInput)
                posts
            ]
        ]


topicsColumn :
    EH.DisplayProfile
    -> String
    -> PublishedPostsDict
    -> Element Msg
topicsColumn dProfile topicSearchStr allPosts =
    let
        talliedTopics : List ( String, ( ( TokenValue, TokenValue ), Int ) )
        talliedTopics =
            let
                findTopic : Published -> Maybe String
                findTopic publishedPost =
                    case publishedPost.core.metadata.context of
                        TopLevel topic ->
                            Just topic

                        Reply postId ->
                            getPublishedPostFromId allPosts postId
                                |> Maybe.andThen findTopic
            in
            allPosts
                |> Dict.values
                |> List.concat
                |> Dict.Extra.filterGroupBy findTopic
                -- This ignores any replies that lead eventually to a postId not in 'posts'
                |> Dict.map
                    (\_ posts ->
                        ( List.foldl
                            (\thisPost ( accBurn, accTip ) ->
                                case thisPost.maybeAccounting of
                                    Just accounting ->
                                        ( TokenValue.add
                                            accounting.totalBurned
                                            accBurn
                                        , TokenValue.add
                                            accounting.totalTipped
                                            accTip
                                        )

                                    Nothing ->
                                        ( TokenValue.add
                                            thisPost.core.authorBurn
                                            accBurn
                                        , accTip
                                        )
                            )
                            ( TokenValue.zero, TokenValue.zero )
                            posts
                        , List.length posts
                        )
                    )
                |> Dict.toList
                |> List.sortBy (Tuple.second >> Tuple.first >> Tuple.first >> TokenValue.toFloatWithWarning >> negate)

        filteredTalliedTopics =
            talliedTopics
                |> List.filter
                    (\( topic, _ ) ->
                        String.contains topicSearchStr topic
                    )

        commonElStyles =
            [ Element.spacing 5
            , padding 5
            , Element.Border.width 1
            , Element.Border.color Theme.almostWhite
            , width fill
            , Element.pointer
            , Element.height <| Element.px 40
            , whiteGlowAttributeSmall
            , Element.Font.color EH.white
            , Element.Background.color EH.black
            ]

        topicEls =
            filteredTalliedTopics
                |> List.map
                    (\( topic, ( ( totalBurned, totalTipped ), count ) ) ->
                        row
                            (commonElStyles
                                ++ [ Element.Events.onClick <|
                                        GotoRoute <|
                                            RouteViewContext <|
                                                Topic topic
                                   ]
                            )
                            [ el
                                [ width <| Element.px 100 ]
                              <|
                                row
                                    [ padding 5
                                    , Element.spacing 3
                                    , Element.Border.rounded 5
                                    , Element.Background.color theme.daiBurnedBackground
                                    , Element.Font.color
                                        (if theme.daiBurnedTextIsWhite then
                                            EH.white

                                         else
                                            EH.black
                                        )
                                    ]
                                    [ daiSymbol theme.daiBurnedTextIsWhite [ Element.height <| Element.px 18 ]
                                    , Element.text <|
                                        (TokenValue.toConciseString totalBurned
                                            |> (if TokenValue.compare totalBurned (TokenValue.fromIntTokenValue 1) == LT then
                                                    String.left 5

                                                else
                                                    identity
                                               )
                                        )
                                    ]
                            , el
                                [ width fill
                                , Element.height fill
                                ]
                              <|
                                el
                                    [ Element.centerY
                                    , Element.Font.color EH.white
                                    ]
                                <|
                                    Element.text topic
                            , el
                                [ Element.alignRight
                                , Element.Font.color EH.white
                                ]
                              <|
                                Element.text <|
                                    String.fromInt count
                            ]
                    )

        exactTopicFound =
            talliedTopics
                |> List.any (Tuple.first >> (==) topicSearchStr)

        maybeCreateTopicEl =
            if topicSearchStr /= "" && not exactTopicFound then
                Just <|
                    el
                        (commonElStyles
                            ++ [ Element.Events.onClick <|
                                    GotoRoute <|
                                        Compose <|
                                            TopLevel topicSearchStr
                               ]
                        )
                    <|
                        row
                            [ Element.centerY
                            ]
                            [ Element.text "Start new topic "
                            , el
                                [ Element.Font.italic
                                , Element.Font.bold
                                ]
                              <|
                                Element.text topicSearchStr
                            ]

            else
                Nothing
    in
    Element.map MsgUp <|
        column
            [ Element.Border.roundEach
                { topRight = 0
                , topLeft = 0
                , bottomRight = 5
                , bottomLeft = 5
                }
            , width (fill |> Element.maximum 530)
            , padding 5
            , Element.spacing 5
            , Element.Background.color EH.black
            ]
            ((Maybe.map List.singleton maybeCreateTopicEl
                |> Maybe.withDefault []
             )
                ++ topicEls
            )


walletUXPane :
    DisplayProfile
    -> Maybe PhaceIconId
    -> String
    -> Common.Types.Wallet
    -> Element Msg
walletUXPane dProfile showAddressId demoPhaceSrc wallet =
    let
        phaceEl =
            case Wallet.userInfo wallet of
                Nothing ->
                    el
                        [ Element.Border.rounded 10
                        , Element.Border.glow
                            (Element.rgba 1 0 1 0.3)
                            9
                        ]
                    <|
                        --phaceElement
                        --( 100, 100 )
                        --True
                        --(Eth.Utils.unsafeToAddress demoPhaceSrc)
                        --(showAddressId == Just DemoPhace)
                        --(ShowOrHideAddress DemoPhace)
                        --NoOp
                        Element.none

                Just userInfo ->
                    --phaceElement
                    --( 100, 100 )
                    --True
                    --userInfo.address
                    --(showAddressId == Just UserPhace)
                    --(ShowOrHideAddress UserPhace)
                    --NoOp
                    --|> el
                    --[ Element.Border.rounded 10
                    --, Element.Border.glow
                    --(Element.rgba 0 0.5 1 0.4)
                    --9
                    --]
                    Element.none

        ( buttonText, maybeButtonAction, maybeExplainerText ) =
            case wallet of
                Common.Types.NoneDetected ->
                    ( "Install Metamask"
                    , Just <| EH.NewTabLink "https://metamask.io/"
                    , Just "Then come back to try on some phaces!"
                    )

                Common.Types.OnlyNetwork _ ->
                    ( "Connect Wallet"
                    , Just <| EH.Action ConnectToWeb3
                    , Just "Each address has a unique phace!"
                    )

                Common.Types.Active userInfo ->
                    let
                        userHasNoEth =
                            userInfo.balance
                                |> Maybe.map TokenValue.isZero
                                |> Maybe.withDefault False
                    in
                    if userHasNoEth then
                        ( "Compose Post"
                        , Nothing
                        , Just "That address has no ETH! You need ETH to post on SmokeSignal."
                        )

                    else
                        ( "Compose Post"
                        , Just <| EH.Action <| GotoRoute <| Compose <| TopLevel Post.defaultTopic
                        , Nothing
                        )

        button =
            let
                attributes =
                    [ paddingXY 10 5
                    , width fill
                    ]
                        ++ (case maybeExplainerText of
                                Nothing ->
                                    [ Element.centerY
                                    ]

                                _ ->
                                    []
                           )
            in
            case maybeButtonAction of
                Just buttonAction ->
                    Theme.unscaryButton
                        dProfile
                        attributes
                        [ buttonText ]
                        buttonAction

                Nothing ->
                    Theme.disabledButton
                        dProfile
                        attributes
                        buttonText

        explainerParagraphOrNone =
            maybeExplainerText
                |> Maybe.map
                    (\text ->
                        Element.paragraph
                            [ Element.Font.color EH.white
                            , Element.Font.size 16
                            ]
                            [ Element.text text ]
                    )
                |> Maybe.withDefault Element.none
    in
    Element.map MsgUp <|
        row
            [ width fill
            , Element.spacing 10
            ]
            [ phaceEl
            , column
                [ width fill
                , Element.spacing 15
                , Element.height fill
                ]
                [ button
                , explainerParagraphOrNone
                ]
            ]


majorSpacing : Int
majorSpacing =
    20


postFeed :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Id
    -> Common.Types.Wallet
    -> List Published
    -> Element Msg
postFeed dProfile donateChecked blockTimes now maybeShowAddressForId wallet listOfPosts =
    listOfPosts
        |> List.sortBy (feedSortByFunc blockTimes now)
        |> List.reverse
        |> List.take 10
        |> List.map
            (previewPost
                dProfile
                donateChecked
                blockTimes
                now
                maybeShowAddressForId
                wallet
                Nothing
            )
        |> column
            [ width fill
            , Element.spacingXY 0 5
            , paddingXY 0 5
            ]


feedSortByFunc :
    Dict Int Time.Posix
    -> Time.Posix
    -> (Published -> Float)
feedSortByFunc blockTimes now =
    \post ->
        let
            postTimeDefaultZero =
                blockTimes
                    |> Dict.get post.id.block
                    |> Maybe.withDefault (Time.millisToPosix 0)

            age =
                TimeHelpers.sub now postTimeDefaultZero

            --ageFactor =
            _ =
                -- 1 at age zero, falls to 0 when 3 days old
                TimeHelpers.getRatio
                    age
                    (TimeHelpers.mul TimeHelpers.oneDay 90)
                    |> clamp 0 1
                    |> (\ascNum -> 1 - ascNum)

            totalBurned =
                Post.totalBurned (PublishedPost post)
                    |> TokenValue.toFloatWithWarning

            newnessMultiplier =
                1

            -- (ageFactor * 4.0) + 1
        in
        totalBurned * newnessMultiplier


previewPost :
    DisplayProfile
    -> Bool
    -> Dict Int Time.Posix
    -> Time.Posix
    -> Maybe Id
    -> Common.Types.Wallet
    -> Maybe PostUX.Model
    -> Published
    -> Element Msg
previewPost dProfile donateChecked blockTimes now maybeShowAddressForId wallet maybePostUXModel post =
    Element.map PostUXMsg <|
        PostPreview.view
            dProfile
            donateChecked
            (maybeShowAddressForId == Just post.id)
            blockTimes
            now
            wallet
            maybePostUXModel
            post
