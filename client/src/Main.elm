module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, checked, href, style, type_)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Route exposing (ImageUrl, Route(..))
import Url.Builder exposing (string)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    ImageUrl


init : Model
init =
    { enableTransitiveReduction = True
    , enableClustering = True
    , route = WholeGraph
    }


type Msg
    = ToggleTransitiveReduction
    | ToggleClustering
    | ChangeImageUrl String
    | SetRoute Route


update : Msg -> Model -> Model
update msg model =
    case Debug.log "" msg of
        ToggleTransitiveReduction ->
            { model | enableTransitiveReduction = not model.enableTransitiveReduction }

        ToggleClustering ->
            { model | enableClustering = not model.enableClustering }

        ChangeImageUrl url ->
            { model | route = Route.parseUrl url }

        SetRoute route ->
            { model | route = route }


view : Model -> Html Msg
view model =
    Html.div []
        [ checkbox ToggleClustering model.enableClustering "clustering"
        , checkbox ToggleTransitiveReduction model.enableTransitiveReduction "transitive reduction"
        , nav model.route
        , Html.div
            [ style "position" "absolute"
            , style "top" "100px"
            , style "left" "0"
            , style "bottom" "0"
            , style "right" "0"
            ]
            [ Html.object
                [ type_ "image/svg+xml"
                , attribute "data" (Route.toUrlString model)
                , style "height" "100%"
                , style "width" "100%"
                , on "load" <| Decode.map ChangeImageUrl <| Decode.at [ "target", "contentWindow", "location", "href" ] Decode.string
                ]
                []
            ]
        ]


nav : Route -> Html Msg
nav route =
    Html.div [] <|
        case route of
            WholeGraph ->
                [ Html.text "Showing entire dependency graph; Click a node to focus on its neighborhood" ]

            NodeContext nodeId ->
                [ Html.div [] [ Html.text <| "Node with ID " ++ String.fromInt nodeId ]
                , Html.a [ href "#", onClick <| SetRoute WholeGraph ] [ Html.text "back to whole graph" ]
                ]


checkbox : Msg -> Bool -> String -> Html Msg
checkbox msg checkedState label =
    Html.label []
        [ Html.input
            [ type_ "checkbox"
            , onClick msg
            , checked checkedState
            ]
            []
        , text label
        ]
