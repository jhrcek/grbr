module Route exposing
    ( ImageUrl
    , Route(..)
    , parseUrl
    , toUrlString
    )

import Url
import Url.Builder exposing (string)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, top)


type Route
    = WholeGraph
    | NodeContext Int


type alias ImageUrl =
    { route : Route
    , enableTransitiveReduction : Bool
    , enableClustering : Bool
    }


parseUrl : String -> Route
parseUrl urlStr =
    Url.fromString urlStr
        |> Maybe.andThen (parse routeParser)
        |> Maybe.withDefault WholeGraph


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map WholeGraph top
        , map NodeContext (s "node" </> int)
        ]


toUrlString : ImageUrl -> String
toUrlString { route, enableTransitiveReduction, enableClustering } =
    let
        path =
            case route of
                WholeGraph ->
                    []

                NodeContext nodeId ->
                    [ "node", String.fromInt nodeId ]
    in
    Url.Builder.absolute path
        [ string "cluster" (showBool enableClustering)
        , string "tred" (showBool enableTransitiveReduction)
        ]


showBool : Bool -> String
showBool b =
    if b then
        "true"

    else
        "false"
