module Camera exposing
    ( Camera
    , lookAt
    )

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


{-| Initialize the camera using look at. Focal length will
always be initialized to 1.
-}
lookAt : Vec3 -> Vec3 -> Vec3 -> Camera
lookAt eye at up =
    let
        forward =
            V3.sub at eye |> V3.normalize

        right =
            V3.cross forward up |> V3.normalize
    in
    { eye = eye
    , forward = forward
    , right = right
    , up = V3.cross right forward
    , focalLength = 1.0
    }
