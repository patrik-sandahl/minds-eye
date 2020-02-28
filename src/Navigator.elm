module Navigator exposing
    ( Camera
    , NavigationState(..)
    , Navigator
    , OrbitState
    , animate
    , cameraEye
    , cameraFocalLength
    , cameraForward
    , cameraRight
    , cameraUp
    , changeResolution
    , init
    )

import Math.Matrix4 as M44 exposing (Mat4)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)


type alias Navigator =
    { resolution : Vec2
    , camera : Camera
    , state : NavigationState
    , mouseMove : Maybe Mouse
    }


type alias OrbitState =
    { origo : Vec3
    , height : Float
    , azimuth : Float
    , elevation : Float
    }


type NavigationState
    = Orbit OrbitState


type alias Mouse =
    { position : Vec2
    }


type alias Camera =
    { eye : Vec3
    , forward : Vec3
    , right : Vec3
    , up : Vec3
    , focalLength : Float
    }


init : NavigationState -> Vec2 -> Navigator
init initialState initialResolution =
    case initialState of
        Orbit state ->
            let
                ( eye, up ) =
                    eyeAndUpFromOrbitState state

                camera =
                    lookAt eye state.origo up 1.0
            in
            { resolution = initialResolution
            , camera = camera
            , state = initialState
            , mouseMove = Nothing
            }


animate : Float -> Navigator -> Navigator
animate playTime navigator =
    case navigator.state of
        Orbit state ->
            let
                newState =
                    { state | azimuth = playTime, elevation = playTime }

                ( eye, up ) =
                    eyeAndUpFromOrbitState newState

                camera =
                    lookAt eye newState.origo up navigator.camera.focalLength
            in
            { navigator | camera = camera, state = Orbit newState }


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


eyeAndUpFromOrbitState : OrbitState -> ( Vec3, Vec3 )
eyeAndUpFromOrbitState state =
    let
        ( xAxis, yAxis, zAxis ) =
            bodyBasisMatrix
                |> rotateMatrixBy state.azimuth state.elevation
                |> getMatrixAxis
    in
    ( V3.scale state.height zAxis, yAxis )


bodyBasisMatrix : Mat4
bodyBasisMatrix =
    M44.makeBasis
        (V3.vec3 1.0 0.0 0.0)
        (V3.vec3 0.0 1.0 0.0)
        (V3.vec3 0.0 0.0 1.0)


getMatrixAxis : Mat4 -> ( Vec3, Vec3, Vec3 )
getMatrixAxis mat =
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


rotateMatrixBy : Float -> Float -> Mat4 -> Mat4
rotateMatrixBy azimuth elevation mat =
    let
        rotAzimuth =
            M44.makeRotate azimuth <| V3.vec3 0.0 1.0 0.0

        rotElevation =
            M44.makeRotate elevation <| V3.vec3 1.0 0.0 0.0

        rotMatrix =
            M44.mul rotElevation rotAzimuth
    in
    M44.mul mat rotMatrix


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


relativeMouseMove : Vec2 -> Vec2 -> Vec2 -> Vec2
relativeMouseMove from to resolution =
    let
        diffX =
            V2.getX to - V2.getX from

        diffY =
            V2.getY to - V2.getY from

        relX =
            diffX / V2.getX resolution

        relY =
            diffY / V2.getY resolution
    in
    if isInfinite relX || isInfinite relY then
        V2.vec2 0.0 0.0

    else
        V2.vec2 relX relY
