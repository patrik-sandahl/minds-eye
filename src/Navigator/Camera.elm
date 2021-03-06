module Navigator.Camera exposing
    ( Camera
    , init
    , rotate
    , uvToRay
    )

import Math.Quaternion as Quaternion exposing (Quaternion)
import Math.Ray as Ray exposing (Ray)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Navigator.Cs as Cs


{-| Representation of a camera.
-}
type alias Camera =
    { eye : Vec3
    , forward : Vec3
    , right : Vec3
    , up : Vec3
    , focalLength : Float
    }


{-| Initialize a camera at world origin, aligned with world axes.
-}
init : Camera
init =
    { eye = V3.vec3 0.0 0.0 0.0
    , forward = Cs.worldXAxis
    , right = Cs.worldZAxis
    , up = Cs.worldYAxis
    , focalLength = 1.0
    }


{-| Rotate the camera.
-}
rotate : Quaternion -> Camera -> Camera
rotate q camera =
    let
        ( forward, up, right ) =
            Quaternion.rotateAxes q ( camera.forward, camera.up, camera.right )
    in
    { camera
        | forward = forward
        , up = up
        , right = right
    }


{-| Create a ray from the normalized UV coordinate.
-}
uvToRay : Vec2 -> Camera -> Ray
uvToRay uv camera =
    -- The y axis must be flipped as the v direction is
    -- opposite to y.
    let
        mid =
            V3.scale camera.focalLength camera.forward
                |> V3.add camera.eye

        u =
            V2.getX uv

        v =
            V2.getY uv

        at =
            V3.add (V3.scale u camera.right) (V3.scale v camera.up |> V3.negate)
                |> V3.add mid

        dir =
            V3.sub at camera.eye
    in
    Ray.init camera.eye dir
