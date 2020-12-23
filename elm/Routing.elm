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
    | ViewContext ViewContext
    | NotFound String


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , (Parser.s "context" </> viewContextParser)
            |> Parser.map (Result.Extra.unpack NotFound ViewContext)
        , (Parser.s "compose" </> postContextParser)
            |> Parser.map (Result.Extra.unpack NotFound Compose)
        ]


routeToString : String -> Route -> String
routeToString basePath route =
    basePath
        ++ (case route of
                Home ->
                    Builder.relative
                        [ "#!" ]
                        []

                Compose context ->
                    Builder.relative
                        ([ "#!", "compose" ] ++ encodePostContextPaths context)
                        (encodePostContextQueryParams context)

                ViewContext context ->
                    Builder.relative
                        ([ "#!", "context" ] ++ encodeViewContextPaths context)
                        (encodeViewContextQueryParams context)

                NotFound _ ->
                    Builder.relative
                        [ "#!" ]
                        []
           )


routeToFullDotEthUrlString : Route -> String
routeToFullDotEthUrlString route =
    routeToString "https://smokesignal.eth/" route


viewContextParser : Parser (Result String ViewContext -> a) a
viewContextParser =
    postContextParser |> Parser.map (Result.map postContextToViewContext)


postContextParser : Parser (Result String Post.Context -> a) a
postContextParser =
    Parser.oneOf
        [ (Parser.s "re" <?> postIdQueryParser)
            |> Parser.map (Result.map Post.Reply)
        , (Parser.s "topic" </> topicParser)
            |> Parser.map (Result.fromMaybe "Couldn't parse topic")
            |> Parser.map (Result.map Post.TopLevel)
        ]


encodeViewContextPaths : ViewContext -> List String
encodeViewContextPaths context =
    context
        |> viewContextToPostContext
        |> encodePostContextPaths


encodePostContextPaths : Post.Context -> List String
encodePostContextPaths context =
    case context of
        Post.Reply _ ->
            [ "re" ]

        Post.TopLevel topic ->
            [ "topic", encodeTopic topic ]


encodeTopic : String -> String
encodeTopic =
    Url.percentEncode


topicParser : Parser (Maybe String -> a) a
topicParser =
    Parser.string
        |> Parser.map Url.percentDecode


encodePostContextQueryParams : Post.Context -> List Builder.QueryParameter
encodePostContextQueryParams context =
    case context of
        Post.Reply postId ->
            encodePostIdQueryParameters postId

        Post.TopLevel _ ->
            []


encodeViewContextQueryParams : ViewContext -> List Builder.QueryParameter
encodeViewContextQueryParams context =
    context
        |> viewContextToPostContext
        |> encodePostContextQueryParams


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
