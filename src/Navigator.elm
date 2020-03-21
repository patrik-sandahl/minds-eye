module Navigator exposing
    ( Mode(..)
    , Navigator
    , camera
    , init
    , panningFrom
    , panningTo
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


{-| Initializing panning from the given uv coordinate.
-}
panningFrom : Vec2 -> Navigator -> Navigator
panningFrom navFrom navigator =
    { navigator | navUv = Just navFrom }


{-| Panning to the following uv coordinate.
-}
panningTo : Vec2 -> Navigator -> Navigator
panningTo navTo navigator =
    case navigator.navUv of
        Just navFrom ->
            case navigator.mode of
                Orbit sphere ->
                    panningToOrbit navFrom navTo sphere navigator

                Surface sphere ->
                    panningToSurface navFrom navTo sphere navigator

        Nothing ->
            navigator


{-| Stop all navigation.
-}
stopNavigate : Navigator -> Navigator
stopNavigate navigator =
    { navigator | navUv = Nothing }


panningToOrbit : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
panningToOrbit navFrom navTo sphere navigator =
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


panningToSurface : Vec2 -> Vec2 -> Sphere -> Navigator -> Navigator
panningToSurface navFrom navTo sphere navigator =
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
