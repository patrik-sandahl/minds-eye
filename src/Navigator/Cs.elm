module Navigator.Cs exposing
    ( worldXAxis
    , worldYAxis
    , worldZAxis
    )

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
