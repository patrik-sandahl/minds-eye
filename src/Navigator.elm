module Navigator exposing
    ( Mode(..)
    , Navigator
    , camera
    , init
    , tick
    )

import Camera exposing (Camera)
import Cs
import Math.Vector3 as V3
import Quaternion
import Sphere exposing (Sphere)


{-| Navigator modes. There can be an orbiting mode, and a surface mode.
-}
type Mode
    = Orbit Sphere
    | Surface Sphere


type alias Navigator =
    { mode : Mode
    , camera : Camera
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


tick : Float -> Navigator -> Navigator
tick t navigator =
    case navigator.mode of
        Orbit sphere ->
            let cam = navigator.camera
                qUp = Quaternion.axisAngle cam.up t
                cam2 = Camera.rotate qUp cam
                qRight = Quaternion.axisAngle cam2.right t
                cam3 = Camera.rotate qRight cam2
                eye = V3.scale defaultOrbitHeightFactor (V3.negate cam3.forward)
                        |> V3.add sphere.origo
                cam4 = { cam3 | eye = eye }
            in { navigator | camera = cam4 }
        _            -> navigator

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
    }


initSurface : Sphere -> Navigator
initSurface sphere =
    { mode = Surface sphere
    , camera = Camera.init
    }


defaultOrbitHeightFactor : Float
defaultOrbitHeightFactor =
    5
