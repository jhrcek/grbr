module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, checked, href, style, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Route exposing (ImageUrl, Route(..))
import Url.Builder exposing (string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


type alias Model =
    { imageUrl : ImageUrl
    , nodeData : List NodeInfo
    , labelSearch : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { imageUrl =
            { enableTransitiveReduction = True
            , enableClustering = True
            , route = WholeGraph
            }
      , nodeData = []
      , labelSearch = ""
      }
    , Http.get
        { url = "/nodes"
        , expect = Http.expectJson processNodesResponse (Decode.list nodeInfoDecoder)
        }
    )


processNodesResponse : Result Http.Error (List NodeInfo) -> Msg
processNodesResponse =
    NodeInfosLoaded << Result.withDefault []


type Msg
    = UpdateImage ImageMsg
    | NodeInfosLoaded (List NodeInfo)


type ImageMsg
    = ToggleTransitiveReduction
    | ToggleClustering
    | ChangeImageUrl String
    | SetRoute Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updatePure msg model, Cmd.none )


updatePure : Msg -> Model -> Model
updatePure msg model =
    case msg of
        UpdateImage imageMsg ->
            { model | imageUrl = updateImageConfig imageMsg model.imageUrl }

        NodeInfosLoaded nodeInfos ->
            { model | nodeData = nodeInfos }


updateImageConfig : ImageMsg -> ImageUrl -> ImageUrl
updateImageConfig msg imageUrl =
    case msg of
        ToggleTransitiveReduction ->
            { imageUrl | enableTransitiveReduction = not imageUrl.enableTransitiveReduction }

        ToggleClustering ->
            { imageUrl | enableClustering = not imageUrl.enableClustering }

        ChangeImageUrl url ->
            { imageUrl | route = Route.parseUrl url }

        SetRoute route ->
            { imageUrl | route = route }


view : Model -> Html Msg
view model =
    Html.div []
        [ checkbox (UpdateImage <| ToggleClustering) model.imageUrl.enableClustering "clustering"
        , checkbox (UpdateImage <| ToggleTransitiveReduction) model.imageUrl.enableTransitiveReduction "transitive reduction"
        , nav model
        , Html.div
            [ style "position" "absolute"
            , style "top" "100px"
            , style "left" "0"
            , style "bottom" "0"
            , style "right" "0"
            ]
            [ Html.object
                [ type_ "image/svg+xml"
                , attribute "data" (Route.toUrlString model.imageUrl)
                , style "height" "100%"
                , style "width" "100%"
                , on "load" <| Decode.map (UpdateImage << ChangeImageUrl) <| Decode.at [ "target", "contentWindow", "location", "href" ] Decode.string
                ]
                []
            ]
        ]


nav : Model -> Html Msg
nav model =
    Html.div [] <|
        case model.imageUrl.route of
            WholeGraph ->
                [ Html.text "Showing entire dependency graph; Click a node to focus on its neighborhood" ]

            NodeContext nodeId ->
                let
                    nodeInfo =
                        List.filter (\n -> n.nodeId == nodeId) model.nodeData
                            |> List.head
                            |> Maybe.withDefault
                                { nodeId = nodeId
                                , nodeLabel = "Unknown node"
                                , nodeGroup = Nothing
                                }
                in
                [ Html.div [] [ Html.text <| "Showing Node with ID " ++ String.fromInt nodeInfo.nodeId ++ " (" ++ nodeInfo.nodeLabel ++ ")" ]
                , Html.a [ href "#", onClick <| UpdateImage <| SetRoute WholeGraph ] [ Html.text "back to whole graph" ]
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


nodeInfoDecoder : Decoder NodeInfo
nodeInfoDecoder =
    Decode.map3 NodeInfo
        (Decode.field "id" Decode.int)
        (Decode.field "label" Decode.string)
        (Decode.field "group" (Decode.nullable Decode.string))


type alias NodeInfo =
    { nodeId : Int
    , nodeLabel : String
    , nodeGroup : Maybe String
    }
