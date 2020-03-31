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

const float FarDistance = 100.0;
const float SurfaceDistance = 0.001;
const vec3 LightSource = vec3(10.0, 0.0, 0.0);

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
        if (d < SurfaceDistance || d0 > FarDistance) break;
    }

    return d0;
}

void main()
{
    vec2 uv = normalizedUV();
    Ray ray = primaryRay(uv);

    vec3 color = vec3(0.0);
    float d = rayMarch(ray);
    if (d < FarDistance) {
        vec3 iPoint = makePoint(ray, d);
        vec3 normal = normalize(iPoint - planetOrigo);
        vec3 lightDir = normalize(LightSource - iPoint);

        float diffuse = max(0.0, dot(normal, lightDir));

        color = vec3(diffuse);
    }

    gl_FragColor = vec4(color, 1.0);
}

    |]
