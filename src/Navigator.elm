module Navigator exposing
    ( Mode(..)
    , Navigator
    , init
    , camera
    )

import Camera exposing (Camera)
import Cs
import Math.Vector3 as V3
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

{-| Get the camera -}
camera : Navigator -> Camera
camera navigator =
    navigator.camera


{-| The initial orbit orientation is along the positive x world axis,
at looking to the center of the sphere.
-}
initOrbit : Sphere -> Navigator
initOrbit sphere =
    let
        eye =
            V3.scale defaultOrbitHeightFactor Cs.worldXAxis 
                |> V3.add sphere.origo
    in
    { mode = Orbit sphere
    , camera = Camera.lookAt eye sphere.origo Cs.worldYAxis
    }


defaultOrbitHeightFactor : Float
defaultOrbitHeightFactor =
    5


initSurface : Sphere -> Navigator
initSurface sphere =
    { mode = Surface sphere
    , camera = Camera.lookAt (V3.vec3 0 0 0) (V3.vec3 0 0 0) (V3.vec3 0 0 0)
    }
