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
            , route = ModuleDepGraph
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
        [ navigation model
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


imageParameterControls : ImageUrl -> Html Msg
imageParameterControls imageUrl =
    Html.div []
        [ checkbox (UpdateImage ToggleClustering) imageUrl.enableClustering "clustering"
        , checkbox (UpdateImage ToggleTransitiveReduction) imageUrl.enableTransitiveReduction "transitive reduction"
        ]


navigation : Model -> Html Msg
navigation model =
    Html.div [] <|
        case model.imageUrl.route of
            PackageDepGraph ->
                [ Html.text "Showing package dependency graph. Back to "
                , moduleGraphLink
                ]

            ModuleDepGraph ->
                [ imageParameterControls model.imageUrl
                , Html.text "Showing module dependency graph. Click a node to focus its neighborhood or go to "
                , packageGraphLink
                ]

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
                [ imageParameterControls model.imageUrl
                , Html.text <| "Showing Node with ID " ++ String.fromInt nodeInfo.nodeId ++ " (" ++ nodeInfo.nodeLabel ++ "). Back to "
                , moduleGraphLink
                , Html.text " or "
                , packageGraphLink
                ]


moduleGraphLink : Html Msg
moduleGraphLink =
    Html.a [ href "#", onClick <| UpdateImage <| SetRoute ModuleDepGraph ] [ Html.text "module dependency graph" ]


packageGraphLink : Html Msg
packageGraphLink =
    Html.a [ href "#", onClick <| UpdateImage <| SetRoute PackageDepGraph ] [ Html.text "package dependency graph" ]


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
