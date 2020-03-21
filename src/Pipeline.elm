module Pipeline exposing
    ( Pipe(..)
    , Pipeline
    , init
    , view
    )

import Navigator.Camera exposing (Camera)
import Html exposing (Html)
import Html.Attributes as Attr
import Math.Sphere exposing (Sphere)
import Math.Vector3 as V3
import Pipeline.Data exposing (Vertex)
import Pipeline.Dev0FragmentShader as Dev0FragmentShader
import Pipeline.QuadVertexShader as QuadVertexShader
import Viewport exposing (Viewport)
import WebGL exposing (Mesh)


{-| The pipeline type.
-}
type alias Pipeline =
    { quadMesh : Mesh Vertex
    }


type Pipe
    = Dev0


{-| Initialize the pipeline.
-}
init : Pipeline
init =
    { quadMesh = makeQuadMesh
    }


{-| View the pipeline using the given pipe.
-}
view : Pipe -> Viewport -> Camera -> Sphere -> Float -> Pipeline -> Html msg
view pipe viewport camera sphere playTime pipeline =
    let
        fragmentShader =
            case pipe of
                Dev0 ->
                    Dev0FragmentShader.program
    in
    WebGL.toHtmlWith
        [ WebGL.antialias
        ]
        [ viewport.width |> Attr.width << floor
        , viewport.height |> Attr.height << floor
        , Attr.style "display" "block"
        ]
        [ WebGL.entity
            QuadVertexShader.program
            fragmentShader
            pipeline.quadMesh
            { resolution = Viewport.resolution viewport
            , playTime = playTime
            , planetOrigo = sphere.origo
            , planetRadius = sphere.radius
            , cameraEye = camera.eye
            , cameraForward = camera.forward
            , cameraRight = camera.right
            , cameraUp = camera.up
            , cameraFocalLength = camera.focalLength
            }
        ]


makeQuadMesh : Mesh Vertex
makeQuadMesh =
    WebGL.triangleStrip
        [ Vertex (V3.vec3 -1.0 1.0 0.0)
        , Vertex (V3.vec3 -1.0 -1.0 0.0)
        , Vertex (V3.vec3 1.0 1.0 0.0)
        , Vertex (V3.vec3 1.0 -1.0 0.0)
        ]
