module Update exposing
    ( init
    , update
    )

import Browser.Dom as Dom
import Navigator.Camera as Camera
import Data exposing (DragState(..), Model, MouseButton(..), Msg(..), Key (..))
import Math.Ray as Ray
import Math.Sphere as Sphere
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Navigator exposing (Mode(..))
import Pipeline
import Task
import Viewport


{-| Application init function.
-}
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
      , showHud = False
      }
    , fetchResolution
    )


{-| Application update function.
-}
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
                            ( Dragging, Navigator.startNavigate uv model.navigator )

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
                | navigator = Navigator.moveTo uv model.navigator
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

        KeyDown RotateKey ->
            let foo = Debug.log "Control pressed (rotate)" 0
            in (model, Cmd.none)

        KeyDown HudToggleKey ->
            ( { model | showHud = not model.showHud }, Cmd.none)

        KeyDown OtherKey ->
            (model, Cmd.none)

        KeyUp RotateKey ->
            let foo = Debug.log "Control released (rotate)" 0
            in (model, Cmd.none)

        KeyUp HudToggleKey ->
            ( model, Cmd.none)

        KeyUp OtherKey ->
            (model, Cmd.none)

        Ignore ->
            ( model
            , Cmd.none
            )


fetchResolution : Cmd Msg
fetchResolution =
    Task.perform
        (\viewport ->
            Viewport.init viewport.viewport.width viewport.viewport.height |> ChangeViewport
        )
        Dom.getViewport


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
