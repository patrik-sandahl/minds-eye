module Camera exposing
    ( Camera
    , init
    , rotate
    )

import Cs
import Math.Vector3 as V3 exposing (Vec3)
import Quaternion exposing (Quaternion)


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
rotate quat camera =
    { camera
        | forward = Quaternion.rotate quat camera.forward
        , right = Quaternion.rotate quat camera.right
        , up = Quaternion.rotate quat camera.up
    }
