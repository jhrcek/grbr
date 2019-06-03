module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Color
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, style, type_)
import Html.Events exposing (on)
import Http
import Json.Decode as Decode exposing (Decoder)
import Route exposing (ImageUrl, Route(..))
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize SetWindowSize


type alias Model =
    { imageUrl : ImageUrl
    , nodeData : Dict NodeId NodeInfo
    , menu : Menu
    , windowSize :
        { width : Int
        , height : Int
        }
    }


type alias Menu =
    { isOpen : Bool
    , searchTerm : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { imageUrl =
            { enableTransitiveReduction = True
            , enableClustering = True
            , route = ModuleDepGraph
            }
      , nodeData = Dict.empty
      , menu =
            { isOpen = False
            , searchTerm = ""
            }
      , windowSize =
            { width = 640
            , height = 480
            }
      }
    , Cmd.batch
        [ loadNodes
        , setInitialWindowSize
        ]
    )


setInitialWindowSize : Cmd Msg
setInitialWindowSize =
    Browser.Dom.getViewport
        |> Task.attempt
            (\result ->
                case result of
                    Ok { viewport } ->
                        SetWindowSize (round viewport.width) (round viewport.height)

                    Err _ ->
                        NoOp
            )


loadNodes : Cmd Msg
loadNodes =
    Http.get
        { url = "/nodes"
        , expect = Http.expectJson processNodesResponse (Decode.list nodeInfoDecoder)
        }


processNodesResponse : Result Http.Error (List NodeInfo) -> Msg
processNodesResponse =
    NodeInfosLoaded << Result.withDefault []


type Msg
    = UpdateImage ImageMsg
    | NodeInfosLoaded (List NodeInfo)
    | SetWindowSize Int Int
    | SetSearchTerm String
    | OpenMenu
    | NoOp


type ImageMsg
    = ToggleTransitiveReduction Bool
    | ToggleClustering Bool
    | ChangeImageUrl String
    | SetRoute Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updatePure msg model, Cmd.none )


updatePure : Msg -> Model -> Model
updatePure msg ({ menu, imageUrl } as model) =
    case msg of
        UpdateImage imageMsg ->
            { model
                | imageUrl = updateImageConfig imageMsg imageUrl
                , menu = { searchTerm = "", isOpen = False }
            }

        NodeInfosLoaded nodeInfos ->
            { model | nodeData = Dict.fromList <| List.map (\i -> ( i.nodeId, i )) nodeInfos }

        SetWindowSize w h ->
            { model | windowSize = { width = w, height = h } }

        SetSearchTerm searchTerm ->
            { model | menu = { menu | searchTerm = searchTerm } }

        OpenMenu ->
            { model | menu = { menu | isOpen = True } }

        NoOp ->
            model


updateImageConfig : ImageMsg -> ImageUrl -> ImageUrl
updateImageConfig msg imageUrl =
    case msg of
        ToggleTransitiveReduction bool ->
            { imageUrl | enableTransitiveReduction = bool }

        ToggleClustering bool ->
            { imageUrl | enableClustering = bool }

        ChangeImageUrl url ->
            { imageUrl | route = Route.parseUrl url }

        SetRoute route ->
            { imageUrl | route = route }


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column []
            [ navbar model
            , Element.html <|
                Html.object
                    [ type_ "image/svg+xml"
                    , attribute "data" (Route.toUrlString model.imageUrl)
                    , style "height" (String.fromInt (model.windowSize.height - navbarHeight) ++ "px")
                    , style "width" (String.fromInt model.windowSize.width ++ "px")
                    , on "load" <| Decode.map (UpdateImage << ChangeImageUrl) <| Decode.at [ "target", "contentWindow", "location", "href" ] Decode.string
                    ]
                    []
            ]


imageParameterControls : ImageUrl -> Element Msg
imageParameterControls imageUrl =
    Element.column [ Element.padding 10 ]
        [ Input.checkbox []
            { onChange = UpdateImage << ToggleClustering
            , icon = Input.defaultCheckbox
            , checked = imageUrl.enableClustering
            , label = Input.labelRight [] (Element.text "clustering")
            }
        , Input.checkbox []
            { onChange = UpdateImage << ToggleTransitiveReduction
            , icon = Input.defaultCheckbox
            , checked = imageUrl.enableTransitiveReduction
            , label = Input.labelRight [] (Element.text "transitive reduction")
            }
        ]


navbar : Model -> Element Msg
navbar model =
    Element.row
        [ Element.height (Element.px navbarHeight)
        , Element.width (Element.px model.windowSize.width)
        , Background.color Color.lightGray
        ]
        [ moduleGraphLink model.imageUrl.route
        , packageGraphLink model.imageUrl.route
        , nodePicker model
        , imageParameterControls model.imageUrl
        ]


navbarHeight : Int
navbarHeight =
    80


moduleGraphLink : Route -> Element Msg
moduleGraphLink currentRoute =
    Input.button
        [ Element.height Element.fill
        , highlightNavWhen (currentRoute == ModuleDepGraph)
        ]
        { onPress = Just <| UpdateImage <| SetRoute ModuleDepGraph
        , label = Element.el [ Element.centerY, Element.padding 10 ] <| Element.text "Module dependencies"
        }


packageGraphLink : Route -> Element Msg
packageGraphLink currentRoute =
    Input.button
        [ Element.height Element.fill
        , highlightNavWhen (currentRoute == PackageDepGraph)
        ]
        { onPress = Just <| UpdateImage <| SetRoute PackageDepGraph
        , label = Element.el [ Element.centerY, Element.padding 10 ] <| Element.text "Package dependencies"
        }


nodePicker : Model -> Element Msg
nodePicker model =
    let
        ( isHighlighted, text ) =
            case model.imageUrl.route of
                NodeContext nodeId ->
                    let
                        nodeInfo =
                            Maybe.withDefault dummyNodeInfo <| Dict.get nodeId model.nodeData
                    in
                    ( True, "Focusing module: " ++ nodeInfo.nodeLabel )

                ModuleDepGraph ->
                    ( False, "Focus module" )

                PackageDepGraph ->
                    ( False, "Focus module" )
    in
    Element.el
        ((if model.menu.isOpen then
            [ Element.below (nodeMenu model.menu model.nodeData) ]

          else
            []
         )
            ++ [ Element.height Element.fill
               , highlightNavWhen isHighlighted
               ]
        )
        (Element.el
            [ Element.centerY
            , Element.padding 10
            , Events.onClick OpenMenu
            ]
            (Element.text text)
        )


nodeMenu : Menu -> Dict NodeId NodeInfo -> Element Msg
nodeMenu menu nodeData =
    let
        toItem : NodeInfo -> Element Msg
        toItem nodeInfo =
            Element.el
                [ Events.onClick (UpdateImage <| SetRoute <| NodeContext nodeInfo.nodeId)
                , Element.padding 2
                ]
                (Element.text nodeInfo.nodeLabel)

        items : List (Element Msg)
        items =
            List.map toItem <|
                List.filter (\i -> String.contains menu.searchTerm i.nodeLabel) <|
                    List.sortBy .nodeLabel <|
                        Dict.values nodeData

        searchBox : Element Msg
        searchBox =
            Input.text []
                { onChange = SetSearchTerm
                , text = menu.searchTerm
                , placeholder = Just (Input.placeholder [] (Element.text "filter modules"))
                , label = Input.labelHidden "Module filter"
                }
    in
    Element.column
        [ Background.color Color.white
        , Border.color Color.black
        , Border.width 2
        ]
        (searchBox :: items)


highlightNavWhen : Bool -> Element.Attribute Msg
highlightNavWhen flag =
    Background.color <|
        if flag then
            Color.lightBlue

        else
            Color.lightGray


nodeInfoDecoder : Decoder NodeInfo
nodeInfoDecoder =
    Decode.map3 NodeInfo
        (Decode.field "id" Decode.int)
        (Decode.field "label" Decode.string)
        (Decode.field "group" (Decode.nullable Decode.string))


type alias NodeId =
    Int


type alias NodeInfo =
    { nodeId : NodeId
    , nodeLabel : String
    , nodeGroup : Maybe String
    }


dummyNodeInfo : NodeInfo
dummyNodeInfo =
    { nodeId = -1
    , nodeLabel = "Node not found"
    , nodeGroup = Nothing
    }
