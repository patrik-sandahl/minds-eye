module Camera exposing (Camera)

import Math.Vector3 as V3 exposing (Vec3)


{-| Representation of a camera.
-}
type alias Camera =
    { eye : Vec3
    , forward : Vec3
    , right : Vec3
    , up : Vec3
    , focalLength : Float
    }
