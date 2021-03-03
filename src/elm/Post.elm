module Post exposing (currentMetadataVersion, defaultTopic, encodePostContent, justBodyContent, messageDataDecoder, metadataDecoder)

{-| Helpers related to Post management.
-}

import Eth.Types exposing (Hex)
import Eth.Utils
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Result.Extra
import String.Extra
import Types exposing (..)


justBodyContent : String -> Content
justBodyContent =
    Content Nothing Nothing


nullMetadata : Metadata
nullMetadata =
    Metadata
        0
        (TopLevel defaultTopic)
        Nothing


defaultTopic : String
defaultTopic =
    "misc"


messageDataDecoder : Int -> Decoder Content
messageDataDecoder metadataVersion =
    Decode.field "m" <|
        if metadataVersion < 3 then
            Decode.map justBodyContent Decode.string

        else
            Decode.map3
                Content
                (Decode.index 0 <| Decode.map String.Extra.nonEmpty <| Decode.string)
                (Decode.index 1 <| Decode.map String.Extra.nonEmpty <| Decode.string)
                (Decode.index 2 Decode.string)


metadataDecoder : Decoder Metadata
metadataDecoder =
    versionDecoder
        |> Decode.andThen versionedMetadataDecoder


versionDecoder : Decoder Int
versionDecoder =
    Decode.maybe
        (Decode.field "v" Decode.int)
        |> Decode.map (Maybe.withDefault 1)


versionedMetadataDecoder : Int -> Decoder Metadata
versionedMetadataDecoder version =
    case version of
        0 ->
            Decode.succeed nullMetadata

        1 ->
            Decode.maybe (Decode.field "re" postIdDecoder)
                |> Decode.map
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
            Decode.field "c" contextDecoder
                |> Decode.map
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
                |> Decode.map
                    (\metadata ->
                        { metadata
                            | maybeDecodeError =
                                Just <| "Unknown metadata version '" ++ String.fromInt version ++ "'. Decoding for version '" ++ String.fromInt currentMetadataVersion ++ "'."
                        }
                    )


currentMetadataDecoder : Int -> Decoder Metadata
currentMetadataDecoder decodedVersion =
    Decode.field "c" contextDecoder
        |> Decode.map
            (\context ->
                Metadata
                    decodedVersion
                    context
                    Nothing
            )


currentMetadataVersion : Int
currentMetadataVersion =
    3


contextDecoder : Decoder Context
contextDecoder =
    Decode.oneOf
        [ Decode.map Reply <| Decode.field "re" postIdDecoder
        , Decode.map TopLevel <| Decode.field "topic" Decode.string
        ]


postIdDecoder : Decoder PostId
postIdDecoder =
    Decode.map2
        PostId
        (Decode.index 0 Decode.int)
        (Decode.index 1 hexDecoder)


hexDecoder : Decoder Hex
hexDecoder =
    Decode.string
        |> Decode.andThen
            (Eth.Utils.toHex
                >> Result.Extra.unpack
                    Decode.fail
                    Decode.succeed
            )


encodePostContent : Draft -> String
encodePostContent draft =
    [ ( "m", encodeContent draft.content )
    , ( "v", Encode.int draft.metadata.metadataVersion )
    , ( "c", encodeContext draft.metadata.context )
    ]
        |> Encode.object
        |> Encode.encode 0
        |> (++) "!smokesignal"


encodeContent : Content -> Value
encodeContent content =
    [ Encode.string <| Maybe.withDefault "" <| content.title
    , Encode.string <| Maybe.withDefault "" <| content.desc
    , Encode.string content.body
    ]
        |> Encode.list identity


encodeContext : Context -> Value
encodeContext context =
    case context of
        Reply postId ->
            Encode.object
                [ ( "re", encodePostId postId ) ]

        TopLevel topic ->
            Encode.object
                [ ( "topic", Encode.string topic ) ]


encodePostId : PostId -> Value
encodePostId postId =
    [ Encode.int postId.block
    , postId.messageHash
        |> Eth.Utils.hexToString
        |> Encode.string
    ]
        |> Encode.list identity
