module Pipeline.QuadVertexShader exposing (program)

import Pipeline.Data exposing (Uniforms, Vertex)
import WebGL exposing (Shader)


{-| The vertex shader program for rendering a quad.
-}
program : Shader Vertex Uniforms {}
program =
    [glsl|

precision highp float;

attribute vec3 position;

void main()
{
    gl_Position = vec4(position, 1.0);
}

    |]
