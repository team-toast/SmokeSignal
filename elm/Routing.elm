module Routing exposing (routeParser, routeToString, urlToRoute)

import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Maybe.Extra exposing (unwrap)
import Result.Extra
import Types exposing (Context(..), Id, Route(..))
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.s "context"
            </> postContextParser
            |> Parser.map (Result.Extra.unpack NotFound identity)

        --, (Parser.s "compose" </> postContextParser)
        --|> Parser.map (Result.Extra.unpack NotFound Compose)
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

                RouteViewContext ->
                    --Builder.relative
                    --([ "#!", "context" ] ++ encodeViewContextPaths context)
                    --(encodeViewContextQueryParams context)
                    Builder.relative
                        [ "#!" ]
                        []

                RouteTopic str ->
                    Builder.relative
                        [ "#!", "context", "topic", Url.percentEncode str ]
                        --(encodeViewContextQueryParams context)
                        []

                RoutePost id ->
                    Builder.relative
                        [ "#!", "context", "re" ]
                        (encodePostIdQueryParameters id)

                NotFound _ ->
                    Builder.relative
                        [ "#!" ]
                        []
           )


routeToFullDotEthUrlString : Route -> String
routeToFullDotEthUrlString route =
    routeToString "https://smokesignal.eth/" route


postContextParser : Parser (Result String Route -> a) a
postContextParser =
    Parser.oneOf
        [ (Parser.s "re" <?> postIdQueryParser)
            |> Parser.map (Result.map RoutePost)
        , (Parser.s "topic" </> topicParser)
            |> Parser.map (Result.fromMaybe "Couldn't parse topic")
            |> Parser.map (Result.map RouteTopic)
        ]



--encodeViewContextPaths : ViewContext -> List String
--encodeViewContextPaths context =
--context
--|> viewContextToPostContext
--|> encodePostContextPaths


encodePostContextPaths : Context -> List String
encodePostContextPaths context =
    case context of
        Reply _ ->
            [ "re" ]

        TopLevel topic ->
            [ "topic", encodeTopic topic ]


encodeTopic : String -> String
encodeTopic =
    Url.percentEncode


topicParser : Parser (Maybe String -> a) a
topicParser =
    Parser.string
        |> Parser.map Url.percentDecode


encodePostContextQueryParams : Context -> List Builder.QueryParameter
encodePostContextQueryParams context =
    case context of
        Reply postId ->
            encodePostIdQueryParameters postId

        TopLevel _ ->
            []



--encodeViewContextQueryParams : ViewContext -> List Builder.QueryParameter
--encodeViewContextQueryParams context =
--context
--|> viewContextToPostContext
--|> encodePostContextQueryParams


postIdQueryParser : Query.Parser (Result String Id)
postIdQueryParser =
    Query.map2
        (Result.map2 Id)
        (Query.int "block"
            |> Query.map (Result.fromMaybe "Can't interpret 'block'")
        )
        (hexQueryParser "hash")


encodePostIdQueryParameters : Id -> List Builder.QueryParameter
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
