module Routing exposing (..)

import Common.Types exposing (..)
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Post exposing (Post)
import Result.Extra
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Route
    = Home
    | Compose Post.Context
    | ViewContext Post.Context
    | NotFound String


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , (Parser.s "context" </> contextParser)
            |> Parser.map (Result.Extra.unpack NotFound ViewContext)
        , (Parser.s "compose" </> contextParser)
            |> Parser.map (Result.Extra.unpack NotFound Compose)
        ]


routeToString : String -> Route -> String
routeToString basePath route =
    basePath
        ++ (case route of
                Home ->
                    Builder.relative
                        [ "#" ]
                        []

                Compose context ->
                    Builder.relative
                        ([ "#", "compose" ] ++ encodeContextPaths context)
                        (encodeContextQueryParams context)

                ViewContext context ->
                    Builder.relative
                        ([ "#", "context" ] ++ encodeContextPaths context)
                        (encodeContextQueryParams context)

                NotFound _ ->
                    Builder.relative
                        [ "#" ]
                        []
           )


routeToFullDotEthUrlString : Route -> String
routeToFullDotEthUrlString route =
    routeToString "https://smokesignal.eth/" route


contextParser : Parser (Result String Post.Context -> a) a
contextParser =
    Parser.oneOf
        [ (Parser.s "re" <?> postIdQueryParser)
            |> Parser.map (Result.map Post.ForPost)
        , (Parser.s "topic" </> Parser.string)
            |> Parser.map Post.ForTopic
            |> Parser.map Ok
        ]


encodeContextPaths : Post.Context -> List String
encodeContextPaths context =
    case context of
        Post.ForPost _ ->
            [ "re" ]

        Post.ForTopic topic ->
            [ "topic", topic ]


encodeContextQueryParams : Post.Context -> List Builder.QueryParameter
encodeContextQueryParams context =
    case context of
        Post.ForPost postId ->
            encodePostIdQueryParameters postId

        Post.ForTopic _ ->
            []


postIdQueryParser : Query.Parser (Result String Post.Id)
postIdQueryParser =
    Query.map2
        (Result.map2 Post.Id)
        (Query.int "block"
            |> Query.map (Result.fromMaybe "Can't interpret 'block'")
        )
        (hexQueryParser "hash")


encodePostIdQueryParameters : Post.Id -> List Builder.QueryParameter
encodePostIdQueryParameters postIdInfo =
    [ Builder.string "block" (String.fromInt postIdInfo.block)
    , Builder.string "hash" (Eth.Utils.hexToString postIdInfo.messageHash)
    ]


hexQueryParser : String -> Query.Parser (Result String Hex)
hexQueryParser label =
    Query.string label
        |> Query.map (Result.fromMaybe <| "Can't find '" ++ label ++ "'")
        |> Query.map (Result.andThen Eth.Utils.toHex)


addressParser : Parser (Address -> a) a
addressParser =
    Parser.custom
        "ADDRESS"
        (Eth.Utils.toAddress >> Result.toMaybe)


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault (NotFound "url not found") (Parser.parse routeParser url)
