module Math.Quaternion exposing
    ( Quaternion
    , axisAngle
    , conjugate
    , mul
    , rotate
    , rotateAxes
    , toQuaternion
    , yawPitchRollAxes
    , zero
    )

import Math.Vector3 as V3 exposing (Vec3)


{-| A quaternion, represented as a four dimensional vector.
-}
type Quaternion
    = Quaternion Vec3 Float


{-| A quaternion representing zero rotations.
-}
zero : Quaternion
zero =
    Quaternion (V3.vec3 0.0 0.0 0.0) 1.0


{-| Rotate the radians theta about axis.
-}
axisAngle : Vec3 -> Float -> Quaternion
axisAngle vec theta =
    let
        halfTheta =
            theta * 0.5

        sinVal =
            sin halfTheta

        cosVal =
            cos halfTheta

        v =
            V3.normalize vec |> V3.scale sinVal
    in
    Quaternion v cosVal


{-| Multiplication of two quaternions (Grassman product). Q is applied first, then P.
-}
mul : Quaternion -> Quaternion -> Quaternion
mul (Quaternion pv ps) (Quaternion qv qs) =
    let
        psQv =
            V3.scale ps qv

        qsPv =
            V3.scale qs pv

        pXv =
            V3.cross pv qv

        v =
            V3.add psQv qsPv |> V3.add pXv

        s =
            (ps * qs) - V3.dot pv qv
    in
    Quaternion v s


{-| Calculate the conjugate for the Quaternion.
-}
conjugate : Quaternion -> Quaternion
conjugate (Quaternion v s) =
    Quaternion (V3.negate v) s


{-| Convert a vector to quaternion format.
-}
toQuaternion : Vec3 -> Quaternion
toQuaternion v =
    Quaternion (V3.normalize v) 0


{-| Rotate a vector by the given quaternion.
-}
rotate : Quaternion -> Vec3 -> Vec3
rotate q v =
    -- The order of multiplications are important! qvq* must be
    -- performed backwards as below.
    let
        qC =
            conjugate q

        vQ =
            toQuaternion v

        (Quaternion vR _) =
            mul q <| mul vQ qC
    in
    vR


{-| Rotate the axes forward, up and right using the quaternion.
-}
rotateAxes : Quaternion -> ( Vec3, Vec3, Vec3 ) -> ( Vec3, Vec3, Vec3 )
rotateAxes q ( forward, up, right ) =
    ( rotate q forward
    , rotate q up
    , rotate q right
    )


{-| Rotate the axes forward, up and right in order yaw, pitch and roll.
-}
yawPitchRollAxes : Float -> Float -> Float -> ( Vec3, Vec3, Vec3 ) -> ( Vec3, Vec3, Vec3 )
yawPitchRollAxes yaw pitch roll ( forward, up, right ) =
    let
        qYaw =
            axisAngle up yaw

        ( forwardY, upY, rightY ) =
            rotateAxes qYaw ( forward, up, right )

        qPitch =
            axisAngle rightY pitch

        ( forwardP, upP, rightP ) =
            rotateAxes qPitch ( forwardY, upY, rightY )

        qRoll =
            axisAngle forwardP roll
    in
    rotateAxes qRoll ( forwardP, upP, rightP )
