module Cs exposing
    ( toAxes
    , worldAxes
    , worldXAxis
    , worldYAxis
    , worldZAxis
    )

import Math.Matrix4 as M44 exposing (Mat4)
import Math.Vector3 as V3 exposing (Vec3)


{-| The x axis for the world.
-}
worldXAxis : Vec3
worldXAxis =
    V3.vec3 1.0 0.0 0.0


{-| The y axis for the world.
-}
worldYAxis : Vec3
worldYAxis =
    V3.vec3 0.0 1.0 0.0


{-| The z axis for the world.
-}
worldZAxis : Vec3
worldZAxis =
    V3.vec3 0.0 0.0 1.0


{-| Make a matrix of the world axes.
-}
worldAxes : Mat4
worldAxes =
    M44.makeBasis
        worldXAxis
        worldYAxis
        worldZAxis


{-| Decompose a matrix into x, y and z axes.
-}
toAxes : Mat4 -> ( Vec3, Vec3, Vec3 )
toAxes mat =
    let
        r =
            M44.toRecord mat

        x =
            V3.vec3 r.m11 r.m21 r.m31

        y =
            V3.vec3 r.m12 r.m22 r.m32

        z =
            V3.vec3 r.m13 r.m23 r.m33
    in
    ( x, y, z )
