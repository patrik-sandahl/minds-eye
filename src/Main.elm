module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize, onAnimationFrameDelta)
import Html exposing (Html, div, text)
import Task

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
    Task.perform (\viewport -> ResizeViewport viewport.viewport.width viewport.viewport.height) getViewport

view : Model -> Html Msg
view model =
    div []
        [
            text <| "w=" ++ String.fromFloat model.viewportWidth ++ ", h=" ++ String.fromFloat model.viewportHeight ++ ", playTime=" ++ String.fromFloat model.playTime
        ]

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
        [ onResize (\w h -> ResizeViewport (toFloat w) (toFloat h))
        , onAnimationFrameDelta AnimateFrame
        ]