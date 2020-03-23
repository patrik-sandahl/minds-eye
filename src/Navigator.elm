module Navigator exposing
    ( Mode(..)
    , Navigator
    , camera
    , init
    , startNavigate
    , moveTo
    , stopNavigate
    )

import Navigator.Camera as Camera exposing (Camera)
import Math.Quaternion as Quaternion
import Math.Sphere exposing (Sphere)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3


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


{-| Stop all navigation.
-}
stopNavigate : Navigator -> Navigator
stopNavigate navigator =
    { navigator | navUv = Nothing }


moveToOrbit : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
moveToOrbit navFrom navTo sphere navigator =
    let
        delta =
            V2.sub navFrom navTo

        yawQ =
            Quaternion.axisAngle navigator.camera.up (V2.getX delta)

        yawCam =
            Camera.rotate yawQ navigator.camera

        pitchQ =
            Quaternion.axisAngle yawCam.right (V2.getY delta)

        pitchCam =
            Camera.rotate pitchQ yawCam

        eye =
            V3.scale defaultOrbitHeightFactor (V3.negate pitchCam.forward)
                |> V3.add sphere.origo

        finalCam =
            { pitchCam | eye = eye }
    in
    { navigator
        | camera = finalCam
        , navUv = Just navTo
    }


moveToSurface : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
moveToSurface navFrom navTo sphere navigator =
    { navigator | navUv = Just navTo }


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
    5
