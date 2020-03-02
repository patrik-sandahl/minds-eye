module ProtoNavigator exposing
    ( Camera
    , NavigationState(..)
    , OrbitState
    , ProtoNavigator
    , beginMouseMove
    , beginMouseRotate
    , cameraEye
    , cameraFocalLength
    , cameraForward
    , cameraRight
    , cameraUp
    , changeResolution
    , endAllMouseAction
    , init
    , mouseTo
    )

import Math.Matrix4 as M44 exposing (Mat4)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)


type alias ProtoNavigator =
    { resolution : Vec2
    , camera : Camera
    , state : NavigationState
    , mouseAction : Maybe MouseAction
    }


type alias OrbitState =
    { origo : Vec3
    , height : Float
    , azimuth : Float
    , elevation : Float
    , yaw : Float
    , pitch : Float
    }


type NavigationState
    = Orbit OrbitState


type MouseAction
    = Move Vec2
    | Rotate Vec2


type alias Camera =
    { eye : Vec3
    , forward : Vec3
    , right : Vec3
    , up : Vec3
    , focalLength : Float
    }


init : NavigationState -> Vec2 -> ProtoNavigator
init initialState initialResolution =
    case initialState of
        Orbit state ->
            let
                ( worldEye, worldUp ) =
                    eyeAndWorldUpFromOrbitState state

                ( camEye, at, camUp ) =
                    lookAtFromEyeAndWorldUp worldEye state.origo worldUp state.yaw state.pitch

                camera =
                    lookAt camEye at camUp 1.0
            in
            { resolution = initialResolution
            , camera = camera
            , state = initialState
            , mouseAction = Nothing
            }


beginMouseMove : Vec2 -> ProtoNavigator -> ProtoNavigator
beginMouseMove pos navigator =
    { navigator | mouseAction = Just (Move pos) }


beginMouseRotate : Vec2 -> ProtoNavigator -> ProtoNavigator
beginMouseRotate pos navigator =
    { navigator | mouseAction = Just (Rotate pos) }


mouseTo : Vec2 -> ProtoNavigator -> ProtoNavigator
mouseTo to navigator =
    case navigator.mouseAction of
        Just action ->
            case action of
                Move from ->
                    moveFromMouse from to navigator

                Rotate from ->
                    rotateFromMouse from to navigator

        Nothing ->
            navigator


endAllMouseAction : ProtoNavigator -> ProtoNavigator
endAllMouseAction navigator =
    { navigator | mouseAction = Nothing }


moveFromMouse : Vec2 -> Vec2 -> ProtoNavigator -> ProtoNavigator
moveFromMouse from to navigator =
    case navigator.state of
        Orbit state ->
            let
                relChange =
                    relativeMouseMove from to navigator.resolution

                azimuthChange =
                    -2.0 * V2.getX relChange

                elevationChange =
                    -2.0 * V2.getY relChange

                newState =
                    { state
                        | azimuth = state.azimuth + azimuthChange
                        , elevation = state.elevation + elevationChange
                    }

                ( worldEye, worldUp ) =
                    eyeAndWorldUpFromOrbitState newState

                ( camEye, at, camUp ) =
                    lookAtFromEyeAndWorldUp worldEye newState.origo worldUp newState.yaw newState.pitch

                camera =
                    lookAt camEye at camUp navigator.camera.focalLength
            in
            { navigator | camera = camera, state = Orbit newState, mouseAction = Just (Move to) }


rotateFromMouse : Vec2 -> Vec2 -> ProtoNavigator -> ProtoNavigator
rotateFromMouse from to navigator =
    case navigator.state of
        Orbit state ->
            let
                relChange =
                    relativeMouseMove from to navigator.resolution

                yawChange =
                    2.0 * V2.getX relChange

                pitchChange =
                    2.0 * V2.getY relChange

                newState =
                    { state
                        | yaw = state.yaw + yawChange
                        , pitch = state.pitch + pitchChange
                    }

                ( worldEye, worldUp ) =
                    eyeAndWorldUpFromOrbitState newState

                ( camEye, at, camUp ) =
                    lookAtFromEyeAndWorldUp worldEye newState.origo worldUp newState.yaw newState.pitch

                camera =
                    lookAt camEye at camUp navigator.camera.focalLength
            in
            { navigator | camera = camera, state = Orbit newState, mouseAction = Just (Rotate to) }


changeResolution : Vec2 -> ProtoNavigator -> ProtoNavigator
changeResolution resolution navigator =
    { navigator | resolution = resolution }


cameraEye : ProtoNavigator -> Vec3
cameraEye navigator =
    navigator.camera.eye


cameraForward : ProtoNavigator -> Vec3
cameraForward navigator =
    navigator.camera.forward


cameraRight : ProtoNavigator -> Vec3
cameraRight navigator =
    navigator.camera.right


cameraUp : ProtoNavigator -> Vec3
cameraUp navigator =
    navigator.camera.up


cameraFocalLength : ProtoNavigator -> Float
cameraFocalLength navigator =
    navigator.camera.focalLength


lookAtFromEyeAndWorldUp : Vec3 -> Vec3 -> Vec3 -> Float -> Float -> ( Vec3, Vec3, Vec3 )
lookAtFromEyeAndWorldUp eye origo up yaw pitch =
    let
        forward =
            V3.sub origo eye
                |> V3.normalize

        nup =
            V3.normalize up

        right =
            V3.cross forward nup

        ( xAxis, yAxis, zAxis ) =
            M44.makeBasis right nup forward
                |> rotateMatrixBy yaw pitch
                |> getMatrixAxis
    in
    ( eye, V3.normalize zAxis, yAxis )


eyeAndWorldUpFromOrbitState : OrbitState -> ( Vec3, Vec3 )
eyeAndWorldUpFromOrbitState state =
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
            M44.mul rotAzimuth rotElevation
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
