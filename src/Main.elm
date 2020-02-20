module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Task
import WebGL


type alias Model =
    { viewportWidth : Float
    , viewportHeight : Float
    , playTime : Float
    }


type Msg
    = ResizeViewport Float Float
    | AnimateFrame Float


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewportWidth = 0.0
      , viewportHeight = 0.0
      , playTime = 0.0
      }
    , fetchViewportSize
    )


fetchViewportSize : Cmd Msg
fetchViewportSize =
    Task.perform (\viewport -> ResizeViewport viewport.viewport.width viewport.viewport.height) Dom.getViewport


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
        [ WebGL.clearColor 1.0 0.0 0.0 1.0
        ]
        [ floor model.viewportWidth |> HtmlAttributes.width
        , floor model.viewportHeight |> HtmlAttributes.height
        , HtmlAttributes.style "display" "block"
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeViewport w h ->
            ( { model | viewportWidth = w, viewportHeight = h }
            , Cmd.none
            )

        AnimateFrame delta ->
            ( { model | playTime = model.playTime + delta }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BrowserEvents.onResize (\w h -> ResizeViewport (toFloat w) (toFloat h))
        , BrowserEvents.onAnimationFrameDelta AnimateFrame
        ]
