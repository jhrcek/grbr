module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, checked, style, type_)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Url.Builder exposing (string)


initialUrl : String
initialUrl =
    "http://localhost:3000"


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { enableTransitiveReduction : Bool
    , enableClustering : Bool
    , iframeUrl : String
    }


init : Model
init =
    { enableTransitiveReduction = True
    , enableClustering = True
    , iframeUrl = initialUrl
    }


type Msg
    = ToggleTransitiveReduction
    | ToggleClustering
    | IframeUrlChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleTransitiveReduction ->
            { model | enableTransitiveReduction = not model.enableTransitiveReduction }

        ToggleClustering ->
            { model | enableClustering = not model.enableClustering }

        IframeUrlChanged url ->
            let
                newUrl =
                    String.split "?" url
                        |> List.head
                        |> Maybe.map
                            (\s ->
                                if String.endsWith "/" s then
                                    String.dropRight 1 s

                                else
                                    s
                            )
                        |> Maybe.withDefault initialUrl
            in
            { model | iframeUrl = newUrl }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.label []
            [ Html.input
                [ type_ "checkbox"
                , onClick ToggleClustering
                , checked model.enableClustering
                ]
                []
            , text "clustering"
            ]
        , Html.label []
            [ Html.input
                [ type_ "checkbox"
                , onClick ToggleTransitiveReduction
                , checked model.enableTransitiveReduction
                ]
                []
            , text "transitive reduction"
            ]
        , Html.object
            [ type_ "image/svg+xml"
            , attribute "data" (buildUrl model)
            , style "height" "100%"
            , style "width" "100%"
            , style "top" "50px"
            , style "left" "0px"
            , style "position" "absolute"
            , on "load" <| Decode.map IframeUrlChanged <| Decode.at [ "target", "contentWindow", "location", "href" ] Decode.string
            ]
            []
        ]


buildUrl : Model -> String
buildUrl model =
    Url.Builder.crossOrigin model.iframeUrl
        []
        [ string "cluster" (showBool model.enableClustering)
        , string "tred" (showBool model.enableTransitiveReduction)
        ]


showBool : Bool -> String
showBool b =
    if b then
        "true"

    else
        "false"
