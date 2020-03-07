module Navigator exposing
    ( Mode(..)
    , Navigator
    , camera
    , init
    )

import Camera exposing (Camera)
import Cs
import Math.Matrix4 as M44 exposing (Mat4)
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
    , relativeOrientationMat : Mat4
    , azimuth : Float
    , elevation : Float
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


{-| Get the camera
-}
camera : Navigator -> Camera
camera navigator =
    navigator.camera


{-| The initial orbit orientation is along the positive x world axis,
at looking to the center of the sphere.
-}
initOrbit : Sphere -> Navigator
initOrbit sphere =
    let
        relativeOrientationMat =
            Cs.rotateWorld 0.0 0.0 Cs.worldAxes
    in
    { mode = Orbit sphere
    , camera = orbitingCamera sphere defaultOrbitHeightFactor relativeOrientationMat
    , relativeOrientationMat = relativeOrientationMat
    , azimuth = 0.0
    , elevation = 0.0
    }


orbitingCamera : Sphere -> Float -> Mat4 -> Camera
orbitingCamera sphere heightFactor mat =
    let
        ( x, y, _ ) =
            Cs.toAxes mat

        eye =
            V3.scale (heightFactor * sphere.radius) x |> V3.add sphere.origo
    in
    Camera.lookAt eye sphere.origo y


defaultOrbitHeightFactor : Float
defaultOrbitHeightFactor =
    5


initSurface : Sphere -> Navigator
initSurface sphere =
    { mode = Surface sphere
    , camera = Camera.lookAt (V3.vec3 0 0 0) (V3.vec3 0 0 0) (V3.vec3 0 0 0)
    , relativeOrientationMat = Cs.worldAxes
    , azimuth = 0.0
    , elevation = 0.0
    }
