module Navigator exposing
    ( Mode(..)
    , Navigator
    , init
    )

import Camera exposing (Camera)
import Sphere exposing (Sphere)


{-| Navigator modes. There can be an orbiting mode, and a surface mode.
-}
type Mode
    = Orbit Sphere
    | Surface Sphere


type alias Navigator =
    { mode : Mode }


{-| Initialize the navigator with its mode.
-}
init : Mode -> Navigator
init mode =
    case mode of
        Orbit sphere ->
            initOrbit sphere

        Surface sphere ->
            initSurface sphere


initOrbit : Sphere -> Navigator
initOrbit sphere =
    { mode = Orbit sphere }


initSurface : Sphere -> Navigator
initSurface sphere =
    { mode = Surface sphere }
