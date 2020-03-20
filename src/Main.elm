module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Camera
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Json.Decode as Decode
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Navigator exposing (Mode(..), Navigator)
import Pipeline exposing (Pipe(..), Pipeline)
import Ray
import Sphere exposing (Sphere)
import Task
import Viewport exposing (Viewport)


type DragState
    = Static
    | Dragging


type alias Model =
    { viewport : Viewport
    , latestFrameTimes : List Float
    , playTime : Float
    , dragState : DragState
    , navigator : Navigator
    , pipeline : Pipeline
    , planet : Sphere
    }


type MouseButton
    = Left
    | Other
    | Any


type Msg
    = ChangeViewport Viewport
    | AnimateFrame Float
    | MouseDown MouseButton Float Float
    | MouseMoveTo Float Float
    | MouseUp MouseButton
    | Ignore


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
    let
        planet =
            Sphere.init (V3.vec3 0.0 0.0 0.0) 1.0
    in
    ( { viewport = Viewport.init 0 0
      , latestFrameTimes = []
      , playTime = 0.0
      , dragState = Static
      , navigator = Orbit planet |> Navigator.init
      , pipeline = Pipeline.init
      , planet = planet
      }
    , fetchResolution
    )


fetchResolution : Cmd Msg
fetchResolution =
    Task.perform
        (\viewport ->
            Viewport.init viewport.viewport.width viewport.viewport.height |> ChangeViewport
        )
        Dom.getViewport


view : Model -> Html Msg
view model =
    Html.div
        []
        [ viewHud model
        , Pipeline.view Dev0 model.viewport model.navigator.camera model.planet model.playTime model.pipeline
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeViewport viewport ->
            ( { model
                | viewport = viewport
              }
            , Cmd.none
            )

        AnimateFrame delta ->
            ( { model
                | latestFrameTimes = delta :: List.take 4 model.latestFrameTimes
                , playTime = model.playTime + delta
              }
            , Cmd.none
            )

        MouseDown Left pageX pageY ->
            -- The drag state is only changed when left click starts within the sphere.
            let
                uv =
                    Viewport.normalizedUV (V2.vec2 pageX pageY) model.viewport

                ipos =
                    intersectPlanet model uv

                ( dragState, navigator ) =
                    case ipos of
                        Just _ ->
                            ( Dragging, Navigator.panningFrom uv model.navigator )

                        Nothing ->
                            ( model.dragState, model.navigator )
            in
            ( { model
                | dragState = dragState
                , navigator = navigator
              }
            , Cmd.none
            )

        MouseDown _ _ _ ->
            ( model
            , Cmd.none
            )

        MouseMoveTo pageX pageY ->
            let
                uv =
                    Viewport.normalizedUV (V2.vec2 pageX pageY) model.viewport
            in
            ( { model
                | navigator = Navigator.panningTo uv model.navigator
              }
            , Cmd.none
            )

        MouseUp _ ->
            ( { model
                | dragState = Static
                , navigator = Navigator.stopNavigate model.navigator
              }
            , Cmd.none
            )

        Ignore ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        staticEvents =
            [ BrowserEvents.onResize
                (\w h ->
                    Viewport.init (toFloat w) (toFloat h)
                        |> ChangeViewport
                )
            , BrowserEvents.onAnimationFrameDelta AnimateFrame
            , BrowserEvents.onMouseDown (Decode.map3 MouseDown decodeMouseButton decodeMouseXPos decodeMouseYPos)
            , BrowserEvents.onMouseUp (Decode.map MouseUp decodeMouseButton)
            , BrowserEvents.onVisibilityChange
                (\v ->
                    if v == BrowserEvents.Hidden then
                        MouseUp Any

                    else
                        Ignore
                )
            ]
    in
    case model.dragState of
        Static ->
            Sub.batch staticEvents

        Dragging ->
            BrowserEvents.onMouseMove (Decode.map2 MouseMoveTo decodeMouseXPos decodeMouseYPos) :: staticEvents |> Sub.batch


decodeMouseButton : Decode.Decoder MouseButton
decodeMouseButton =
    Decode.map
        (\v ->
            case v of
                0 ->
                    Left

                _ ->
                    Other
        )
        (Decode.field "button" Decode.int)


decodeMouseXPos : Decode.Decoder Float
decodeMouseXPos =
    Decode.field "pageX" Decode.float


decodeMouseYPos : Decode.Decoder Float
decodeMouseYPos =
    Decode.field "pageY" Decode.float


calcFps : List Float -> Float
calcFps latestFrameTimes =
    if not (List.isEmpty latestFrameTimes) then
        let
            avg =
                List.sum latestFrameTimes / toFloat (List.length latestFrameTimes)
        in
        if avg /= 0.0 then
            1000.0 / avg

        else
            0.0

    else
        0.0


intersectPlanet : Model -> Vec2 -> Maybe Vec3
intersectPlanet model uv =
    let
        ray =
            Camera.uvToRay uv model.navigator.camera

        d =
            Sphere.intersect ray model.planet
    in
    case d of
        Just dd ->
            Ray.pointAt dd ray |> Just

        Nothing ->
            Nothing


viewHud : Model -> Html Msg
viewHud model =
    Html.div
        [ HtmlAttributes.style "display" "inline-block"
        , HtmlAttributes.style "position" "fixed"
        , HtmlAttributes.style "left" "10px"
        , HtmlAttributes.style "top" "10px"
        , HtmlAttributes.style "font-family" "sans-serif"
        , HtmlAttributes.style "font-size" "16px"
        , HtmlAttributes.style "color" "white"
        , HtmlAttributes.style "z-index" "1"
        ]
        [ let
            res =
                String.fromFloat model.viewport.width ++ "x" ++ String.fromFloat model.viewport.height ++ "px"

            fps =
                String.fromInt (calcFps model.latestFrameTimes |> round) ++ " FPS"

            eye =
                model.navigator.camera.eye

            eyePos =
                "Eye x="
                    ++ (V3.getX eye |> String.fromFloat)
                    ++ ", y="
                    ++ (V3.getY eye |> String.fromFloat)
                    ++ ", z="
                    ++ (V3.getZ eye |> String.fromFloat)
          in
          res ++ " " ++ fps ++ " " ++ eyePos |> Html.text
        ]
