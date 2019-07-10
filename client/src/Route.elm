module Route exposing
    ( ImageUrl
    , Route(..)
    , parseUrl
    , toUrlString
    )

import Url
import Url.Builder exposing (string)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s)


type Route
    = ModuleDepGraph
    | PackageDepGraph
    | ModuleContext Int


type alias ImageUrl =
    { route : Route
    , enableTransitiveReduction : Bool
    , enableClustering : Bool
    }


parseUrl : String -> Route
parseUrl urlStr =
    Url.fromString urlStr
        |> Maybe.andThen (parse routeParser)
        |> Maybe.withDefault ModuleDepGraph


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ModuleDepGraph (s "modules")
        , map ModuleContext (s "modules" </> int)
        , map PackageDepGraph (s "packages")
        ]


toUrlString : ImageUrl -> String
toUrlString { route, enableTransitiveReduction, enableClustering } =
    Url.Builder.absolute (routePieces route)
        [ string "cluster" (showBool enableClustering)
        , string "tred" (showBool enableTransitiveReduction)
        ]


routePieces : Route -> List String
routePieces route =
    case route of
        ModuleDepGraph ->
            [ "modules" ]

        PackageDepGraph ->
            [ "packages" ]

        ModuleContext nodeId ->
            [ "modules", String.fromInt nodeId ]


showBool : Bool -> String
showBool b =
    if b then
        "true"

    else
        "false"
