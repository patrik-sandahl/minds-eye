module Pipeline.Dev1FragmentShader exposing (program)

import Pipeline.Data exposing (Uniforms)
import WebGL exposing (Shader)


{-| Dev1 pipeline fragment shader - for development of fBM stuff.
-}
program : Shader {} Uniforms {}
program =
    [glsl|

precision highp float;

uniform vec2 resolution;
uniform float playTime;

uniform vec3 planetOrigo;
uniform float planetRadius;

uniform vec3 cameraEye;
uniform vec3 cameraForward;
uniform vec3 cameraRight;
uniform vec3 cameraUp;
uniform float cameraFocalLength;

void main()
{
    gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
}

    |]
