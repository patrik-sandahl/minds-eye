module Pipeline.Dev0FragmentShader exposing (program)

import Pipeline.Data exposing (Uniforms)
import WebGL exposing (Shader)


{-| Dev0 pipeline fragment shader - for development of orbiting navigation.
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

struct Ray {
    vec3 origin;
    vec3 direction;
};

Ray makeRay(vec3 origin, vec3 direction)
{
    return Ray(origin, normalize(direction));
}

vec3 makePoint(Ray ray, float d)
{
    return ray.origin + ray.direction * d;
}

Ray primaryRay(vec2 uv)
{
    vec3 center = cameraEye + cameraForward * cameraFocalLength;
    vec3 spot = center + cameraRight * uv.x + cameraUp * uv.y;

    return makeRay(cameraEye, spot - cameraEye);
}

vec2 normalizedUV()
{
    return (gl_FragCoord.xy - 0.5 * resolution) / min(resolution.x, resolution.y);
}

float sphere(vec3 pos, float radius)
{
    return length(pos) - radius;
}

float intersectScene(vec3 p)
{
    return sphere(p - planetOrigo, planetRadius);
}

float rayMarch(Ray ray)
{
    float d0 = 0.0;
    for (int i = 0; i < 100; ++i) {
        vec3 p = makePoint(ray, d0);
        float d = intersectScene(p);

        d0 += d;
        if (d < 0.001 || d0 > 30.0) break;
    }

    return d0;
}

void main()
{
    //vec2 uv = fract(normalizedUV() * 10.0);
    vec2 uv = normalizedUV();
    Ray ray = primaryRay(uv);

    float d = rayMarch(ray);

    vec3 color = vec3(0.3);
    if (d < 30.0) {
        vec3 p = makePoint(ray, d);
        vec3 dir = p - planetOrigo;
        float u = (dir.x / length(dir) + 1.0) * 0.5;
        float v = (dir.y / length(dir) + 1.0) * 0.5;

        if (u > 0.49 && u < 0.51) color = vec3(1.0);
        else if (v > 0.49 && v < 0.51) color = vec3(0.0);
        else color = vec3(u, v, 0.0);
    }

    gl_FragColor = vec4(color, 1.0);

    //gl_FragColor = vec4(uv.x, uv.y, 0.0, 1.0);
    //gl_FragColor = vec4(ray.direction, 1.0);
}

    |]
