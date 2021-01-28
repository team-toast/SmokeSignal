module Post exposing (decodePostData)

{-| Helpers related to Post management.
-}

import Context exposing (Context)
import Eth.Types exposing (Hex)
import Eth.Utils
import Json.Decode as Decode exposing (Decoder)
import Result.Extra
import String.Extra
import Types exposing (..)


decodePostData : String -> ( Metadata, Content )
decodePostData str =
    case ( String.left 12 str, String.dropLeft 12 str ) of
        ( "!smokesignal", jsonStr ) ->
            jsonStr
                |> Decode.decodeString postDataDecoder
                |> Result.Extra.extract
                    (\decodeErr ->
                        ( { nullMetadata
                            | maybeDecodeError =
                                Decode.errorToString decodeErr
                                    |> Just
                          }
                        , justBodyContent jsonStr
                        )
                    )

        _ ->
            ( nullMetadata
            , justBodyContent str
            )


justBodyContent : String -> Content
justBodyContent =
    Content Nothing Nothing


nullMetadata : Metadata
nullMetadata =
    Metadata
        0
        (Context.TopLevel defaultTopic)
        Nothing


postDataDecoder : Decoder ( Metadata, Content )
postDataDecoder =
    metadataDecoder
        |> Decode.andThen
            (\metadata ->
                Decode.map
                    (Tuple.pair metadata)
                    (messageDataDecoder metadata.metadataVersion)
            )


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
                                    (Context.Reply replyTo)
                                    Nothing

                            Nothing ->
                                Metadata
                                    version
                                    (Context.TopLevel <| defaultTopic)
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
        [ Decode.map Context.Reply <| Decode.field "re" postIdDecoder
        , Decode.map Context.TopLevel <| Decode.field "topic" Decode.string
        ]


postIdDecoder : Decoder Context.PostId
postIdDecoder =
    Decode.map2
        Context.PostId
        (Decode.index 0 Decode.int)
        (Decode.index 1 hexDecoder)


hexDecoder : Decoder Hex
hexDecoder =
    Decode.string
        |> Decode.map Eth.Utils.toHex
        |> Decode.andThen
            (\result ->
                case result of
                    Err errStr ->
                        Decode.fail errStr

                    Ok hex ->
                        Decode.succeed hex
            )
