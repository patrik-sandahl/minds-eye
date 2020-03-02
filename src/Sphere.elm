module Sphere exposing
    ( Sphere
    , init
    , intersect
    )

import Math.Vector3 as V3 exposing (Vec3)
import Ray exposing (Ray)


{-| A sphere represented with an origo and a radius.
-}
type alias Sphere =
    { origo : Vec3
    , radius : Float
    }


{-| Initialize the sphere.
-}
init : Vec3 -> Float -> Sphere
init origo radius =
    { origo = origo
    , radius = radius
    }


{-| Calculating the ray intersect of the sphere.
-}
intersect : Ray -> Sphere -> Maybe Float
intersect ray sphere =
    let
        e =
            V3.sub ray.origin sphere.origo

        rSq =
            sphere.radius * sphere.radius

        eSq =
            V3.lengthSquared e

        a =
            V3.dot e ray.direction

        bSq =
            eSq - (a * a)

        f =
            rSq - bSq |> sqrt
    in
    if rSq - (eSq - (a * a)) < 0.0 then
        Nothing

    else if eSq < rSq then
        a + f |> Just

    else
        a - f |> Just
