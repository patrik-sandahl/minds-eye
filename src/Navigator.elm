module Navigator exposing
    ( Camera
    , Navigator
    , cameraEye
    , cameraFocalLength
    , cameraForward
    , cameraRight
    , cameraUp
    , changeResolution
    , init
    )

import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)


type alias Navigator =
    { resolution : Vec2
    , camera : Camera
    }


type alias Camera =
    { eye : Vec3
    , forward : Vec3
    , right : Vec3
    , up : Vec3
    , focalLength : Float
    }


init : Navigator
init =
    { resolution = V2.vec2 0.0 0.0
    , camera = lookAt (V3.vec3 5.0 0.0 0.0) (V3.vec3 0.0 0.0 0.0) (V3.vec3 0.0 1.0 0.0) 1.0
    }


changeResolution : Vec2 -> Navigator -> Navigator
changeResolution resolution navigator =
    { navigator | resolution = resolution }


cameraEye : Navigator -> Vec3
cameraEye navigator =
    navigator.camera.eye


cameraForward : Navigator -> Vec3
cameraForward navigator =
    navigator.camera.forward


cameraRight : Navigator -> Vec3
cameraRight navigator =
    navigator.camera.right


cameraUp : Navigator -> Vec3
cameraUp navigator =
    navigator.camera.up


cameraFocalLength : Navigator -> Float
cameraFocalLength navigator =
    navigator.camera.focalLength


lookAt : Vec3 -> Vec3 -> Vec3 -> Float -> Camera
lookAt eye at upDir focalLength =
    let
        forward =
            V3.sub at eye |> V3.normalize

        right =
            V3.cross forward upDir |> V3.normalize

        up =
            V3.cross right forward
    in
    { eye = eye, forward = forward, right = right, up = up, focalLength = focalLength }
