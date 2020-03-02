module Navigator exposing
    ( Mode(..)
    , Navigator
    , init
    )

import Sphere exposing (Sphere)


{-| Navigator modes. There can be an orbiting mode, and an exploring mode.
-}
type Mode
    = Orbit Sphere
    | Explore Sphere


type alias Navigator =
    {}


{-| Initialize the navigator with its mode.
-}
init : Mode -> Navigator
init mode =
    {}
