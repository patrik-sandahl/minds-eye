module Navigator exposing
    ( Mode(..)
    , Navigator
    , camera
    , init
    , moveTo
    , rotateTo
    , startNavigate
    , stopNavigate
    )

import Math.Quaternion as Quaternion
import Math.Sphere exposing (Sphere)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3
import Navigator.Camera as Camera exposing (Camera)


{-| Navigator modes. There can be an orbiting mode, and a surface mode.
-}
type Mode
    = Orbit Sphere
    | Surface Sphere


type alias Navigator =
    { mode : Mode
    , camera : Camera
    , navUv : Maybe Vec2
    }


type RotateDirection
    = Left
    | Right


{-| Initialize the navigator with its mode.
-}
init : Mode -> Navigator
init mode =
    case mode of
        Orbit sphere ->
            initOrbit sphere

        Surface sphere ->
            initSurface sphere


{-| Initializing navigation action from this uv.
-}
startNavigate : Vec2 -> Navigator -> Navigator
startNavigate navFrom navigator =
    { navigator | navUv = Just navFrom }


{-| Move the camera according to the new uv.
-}
moveTo : Vec2 -> Navigator -> Navigator
moveTo navTo navigator =
    case navigator.navUv of
        Just navFrom ->
            case navigator.mode of
                Orbit sphere ->
                    moveToOrbit navFrom navTo sphere navigator

                Surface sphere ->
                    moveToSurface navFrom navTo sphere navigator

        Nothing ->
            navigator


{-| Rotate the camera according to the new uv.
-}
rotateTo : Vec2 -> Navigator -> Navigator
rotateTo navTo navigator =
    case navigator.navUv of
        Just navFrom ->
            case navigator.mode of
                Orbit sphere ->
                    rotateToOrbit navFrom navTo sphere navigator

                Surface sphere ->
                    rotateToSurface navFrom navTo sphere navigator

        Nothing ->
            navigator


{-| Stop all navigation.
-}
stopNavigate : Navigator -> Navigator
stopNavigate navigator =
    { navigator | navUv = Nothing }


moveToOrbit : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
moveToOrbit navFrom navTo sphere navigator =
    let
        inCamera =
            navigator.camera

        delta =
            V2.sub navFrom navTo

        yaw =
            V2.getX delta

        pitch =
            V2.getY delta

        ( forward, up, right ) =
            Quaternion.yawPitchRollAxes yaw pitch 0.0 ( inCamera.forward, inCamera.up, inCamera.right )

        eye =
            V3.scale defaultOrbitHeightFactor (V3.negate forward)
                |> V3.add sphere.origo

        newCamera =
            { inCamera
                | eye = eye
                , forward = forward
                , up = up
                , right = right
            }
    in
    { navigator
        | camera = newCamera
        , navUv = Just navTo
    }


moveToSurface : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
moveToSurface navFrom navTo sphere navigator =
    { navigator | navUv = Just navTo }


rotateToOrbit : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
rotateToOrbit navFrom navTo sphere navigator =
    let
        roll =
            case rotateDirection navFrom navTo of
                Left ->
                    V2.distance navFrom navTo |> negate

                Right ->
                    V2.distance navFrom navTo

        inCamera =
            navigator.camera

        ( forward, up, right ) =
            Quaternion.yawPitchRollAxes 0.0 0.0 roll ( inCamera.forward, inCamera.up, inCamera.right )

        newCamera =
            { inCamera
                | forward = forward
                , up = up
                , right = right
            }
    in
    { navigator
        | camera = newCamera
        , navUv = Just navTo
    }


rotateToSurface : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
rotateToSurface navFrom navTo sphere navigator =
    { navigator | navUv = Just navTo }


rotateDirection : Vec2 -> Vec2 -> RotateDirection
rotateDirection navFrom navTo =
    let
        fromAngle =
            atan2 (V2.getX navFrom) (V2.getY navFrom)

        toAngle =
            atan2 (V2.getX navTo) (V2.getY navTo)
    in
    if fromAngle < toAngle then
        Right

    else
        Left


{-| Get the camera
-}
camera : Navigator -> Camera
camera navigator =
    navigator.camera


initOrbit : Sphere -> Navigator
initOrbit sphere =
    let
        cam =
            Camera.init

        eye =
            V3.scale defaultOrbitHeightFactor (V3.negate cam.forward)
                |> V3.add sphere.origo
    in
    { mode = Orbit sphere
    , camera = { cam | eye = eye }
    , navUv = Nothing
    }


initSurface : Sphere -> Navigator
initSurface sphere =
    { mode = Surface sphere
    , camera = Camera.init
    , navUv = Nothing
    }


defaultOrbitHeightFactor : Float
defaultOrbitHeightFactor =
    3
