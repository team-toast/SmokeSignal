module Routing exposing (..)

import Context exposing (Context)
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Result.Extra
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Route
    = Home
    | Compose Context
    | ViewContext Context
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
                        [ "#!" ]
                        []

                Compose context ->
                    Builder.relative
                        ([ "#!", "compose" ] ++ encodeContextPaths context)
                        (encodePostContextQueryParams context)

                ViewContext context ->
                    Builder.relative
                        ([ "#!", "context" ] ++ encodeContextPaths context)
                        (encodeContextQueryParams context)

                -- Topic str ->
                --     Builder.relative
                --         [ "#!", "context", "topic", Url.percentEncode <| String.toLower str ]
                --         --(encodeViewContextQueryParams context)
                --         []
                -- Post id ->
                --     Builder.relative
                --         [ "#!", "context", "re" ]
                --         (encodePostIdQueryParameters id)
                NotFound _ ->
                    Builder.relative
                        [ "#!" ]
                        []
           )


routeToFullDotEthUrlString : Route -> String
routeToFullDotEthUrlString route =
    routeToString "https://smokesignal.eth/" route


contextParser : Parser (Result String Context -> a) a
contextParser =
    Parser.oneOf
        [ (Parser.s "re" <?> postIdQueryParser)
            |> Parser.map (Result.map Context.Reply)
        , (Parser.s "topic" </> topicParser)
            |> Parser.map (Result.fromMaybe "Couldn't parse topic")
            |> Parser.map (Result.map Context.TopLevel)
        ]



encodeContextPaths : Context -> List String
encodeContextPaths context =
    case context of
        Context.Reply _ ->
            [ "re" ]

        Context.TopLevel topic ->
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
        Context.Reply postId ->
            encodePostIdQueryParameters postId

        Context.TopLevel _ ->
            []


encodeContextQueryParams : Context -> List Builder.QueryParameter
encodeContextQueryParams context =
    context
        |> encodePostContextQueryParams


postIdQueryParser : Query.Parser (Result String Context.PostId)
postIdQueryParser =
    Query.map2
        (Result.map2 Context.PostId)
        (Query.int "block"
            |> Query.map (Result.fromMaybe "Can't interpret 'block'")
        )
        (hexQueryParser "hash")


encodePostIdQueryParameters : Context.PostId -> List Builder.QueryParameter
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
