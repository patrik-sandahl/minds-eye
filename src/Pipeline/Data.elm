module Pipeline.Data exposing
    ( Uniforms
    , Vertex
    )

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)


{-| Vertex type - just the position.
-}
type alias Vertex =
    { position : Vec3
    }


{-| Shader uniforms.
-}
type alias Uniforms =
    { resolution : Vec2
    , playTime : Float
    , planetOrigo : Vec3
    , planetRadius : Float
    , cameraEye : Vec3
    , cameraForward : Vec3
    , cameraRight : Vec3
    , cameraUp : Vec3
    , cameraFocalLength : Float
    }
