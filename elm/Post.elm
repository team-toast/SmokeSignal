module Post exposing (..)

import Common.Types exposing (Content, Context(..), Core, Draft, EncodedDraft, Id, Metadata, Post(..))
import Element exposing (Element)
import Element.Font
import ElementMarkdown
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.List as ListHelpers
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra
import Result.Extra
import String.Extra
import Theme exposing (Theme)
import TokenValue exposing (TokenValue)


justBodyContent : String -> Content
justBodyContent =
    Content Nothing Nothing


getCore : Post -> Core
getCore post =
    case post of
        PublishedPost p ->
            p.core

        PostDraft d ->
            d.core


totalBurned : Post -> TokenValue
totalBurned post =
    case post of
        PublishedPost publishedPost ->
            case publishedPost.maybeAccounting of
                Just accounting ->
                    accounting.totalBurned

                Nothing ->
                    publishedPost.core.authorBurn

        PostDraft postDraft ->
            postDraft.core.authorBurn


totalTipped : Post -> TokenValue
totalTipped post =
    case post of
        PublishedPost publishedPost ->
            case publishedPost.maybeAccounting of
                Just accounting ->
                    accounting.totalTipped

                Nothing ->
                    TokenValue.zero

        _ ->
            TokenValue.zero


buildMetadataFromContext : Context -> Metadata
buildMetadataFromContext context =
    versionedMetadataWithDefault <| Just context


contextReplyTo : Context -> Maybe Id
contextReplyTo context =
    case context of
        Reply id ->
            Just id

        _ ->
            Nothing


contextTopic : Context -> Maybe String
contextTopic context =
    case context of
        TopLevel topic ->
            Just topic

        _ ->
            Nothing


defaultTopic : String
defaultTopic =
    "misc"


nullMetadata : Metadata
nullMetadata =
    Metadata
        0
        (TopLevel defaultTopic)
        Nothing


currentMetadataVersion : Int
currentMetadataVersion =
    3


versionedMetadataWithDefault : Maybe Context -> Metadata
versionedMetadataWithDefault maybeContext =
    Metadata
        currentMetadataVersion
        (maybeContext |> Maybe.withDefault (TopLevel defaultTopic))
        Nothing


blankVersionedMetadata : Metadata
blankVersionedMetadata =
    versionedMetadataWithDefault Nothing


encodeDraft : Draft -> EncodedDraft
encodeDraft draft =
    EncodedDraft
        draft.core.author
        ("!smokesignal" ++ encodeToString ( draft.core.metadata, draft.core.content ))
        draft.core.authorBurn
        draft.donateAmount


encodeToString : ( Metadata, Content ) -> String
encodeToString ( metadata, content ) =
    E.encode 0
        (E.object <|
            [ ( "m", encodeContent content )
            , ( "v", E.int metadata.metadataVersion )
            , ( "c", encodeContext metadata.context )
            ]
        )


decodePostData : String -> ( Metadata, Content )
decodePostData src =
    src
        |> D.decodeString postDataDecoder
        |> Result.Extra.extract
            (\decodeErr ->
                ( { nullMetadata
                    | maybeDecodeError =
                        Just <|
                            D.errorToString decodeErr
                  }
                , justBodyContent src
                )
            )


postDataDecoder : D.Decoder ( Metadata, Content )
postDataDecoder =
    metadataDecoder
        |> D.andThen
            (\metadata ->
                D.map
                    (Tuple.pair metadata)
                    (messageDataDecoder metadata.metadataVersion)
            )


metadataDecoder : D.Decoder Metadata
metadataDecoder =
    versionDecoder
        |> D.andThen
            (\v -> versionedMetadataDecoder v)


versionDecoder : D.Decoder Int
versionDecoder =
    D.maybe
        (D.field "v" D.int)
        |> D.map (Maybe.withDefault 1)


versionedMetadataDecoder : Int -> D.Decoder Metadata
versionedMetadataDecoder version =
    case version of
        0 ->
            D.succeed nullMetadata

        1 ->
            D.maybe (D.field "re" postIdDecoder)
                |> D.map
                    (\maybeReplyTo ->
                        case maybeReplyTo of
                            Just replyTo ->
                                Metadata
                                    version
                                    (Reply replyTo)
                                    Nothing

                            Nothing ->
                                Metadata
                                    version
                                    (TopLevel <| defaultTopic)
                                    Nothing
                    )

        2 ->
            D.field "c" contextDecoder
                |> D.map
                    (\context ->
                        Metadata
                            version
                            context
                            Nothing
                    )

        3 ->
            currentMetadataDecoder version

        _ ->
            currentMetadataDecoder version
                |> D.map
                    (\metadata ->
                        { metadata
                            | maybeDecodeError =
                                Just <| "Unknown metadata version '" ++ String.fromInt version ++ "'. Decoding for version '" ++ String.fromInt currentMetadataVersion ++ "'."
                        }
                    )


currentMetadataDecoder : Int -> D.Decoder Metadata
currentMetadataDecoder decodedVersion =
    D.field "c" contextDecoder
        |> D.map
            (\context ->
                Metadata
                    decodedVersion
                    context
                    Nothing
            )


encodeContext : Context -> E.Value
encodeContext context =
    case context of
        Reply postId ->
            E.object
                [ ( "re", encodePostId postId ) ]

        TopLevel topic ->
            E.object
                [ ( "topic", E.string topic ) ]


contextDecoder : D.Decoder Context
contextDecoder =
    D.oneOf
        [ D.map Reply <| D.field "re" postIdDecoder
        , D.map TopLevel <| D.field "topic" D.string
        ]


encodePostId : Id -> E.Value
encodePostId postId =
    E.list identity
        [ E.int postId.block
        , encodeHex postId.messageHash
        ]


postIdDecoder : D.Decoder Id
postIdDecoder =
    D.map2
        Id
        (D.index 0 D.int)
        (D.index 1 hexDecoder)


encodeContent : Content -> E.Value
encodeContent content =
    E.list identity
        [ E.string <| Maybe.withDefault "" <| content.title
        , E.string <| Maybe.withDefault "" <| content.desc
        , E.string content.body
        ]


messageDataDecoder : Int -> D.Decoder Content
messageDataDecoder metadataVersion =
    D.field "m" <|
        if metadataVersion < 3 then
            D.map justBodyContent D.string

        else
            D.map3
                Content
                (D.index 0 <| D.map String.Extra.nonEmpty <| D.string)
                (D.index 1 <| D.map String.Extra.nonEmpty <| D.string)
                (D.index 2 D.string)


encodeHex : Hex -> E.Value
encodeHex =
    Eth.Utils.hexToString >> E.string


hexDecoder : D.Decoder Hex
hexDecoder =
    D.string
        |> D.map Eth.Utils.toHex
        |> D.andThen
            (\result ->
                case result of
                    Err errStr ->
                        D.fail errStr

                    Ok hex ->
                        D.succeed hex
            )


sanitizeTopic : String -> String
sanitizeTopic =
    String.toLower >> String.Extra.clean >> String.replace " " "-"


contentIsEmpty : Content -> Bool
contentIsEmpty content =
    ((content.title |> Maybe.withDefault "") == "") && content.body == ""
