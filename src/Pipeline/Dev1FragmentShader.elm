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

vec4 intersectScene(vec3 p)
{
    float d = sphere(p - planetOrigo, planetRadius);

    return vec4(d, 0.0, 0.0, 0.0);
}

vec4 rayMarch(Ray ray)
{
    float d0 = 0.0;
    vec4 v = vec4(0.0);
    for (int i = 0; i < 100; ++i) {
        vec3 p = makePoint(ray, d0);
        v = intersectScene(p);

        d0 += v.x;
        if (v.x < SurfaceDistance || d0 > FarDistance) break;
    }

    v.x = d0;
    return v;
}

vec4 noised(vec3 x);

void main()
{
    vec2 uv = normalizedUV();
    Ray ray = primaryRay(uv);

    vec3 color = vec3(0.0);
    vec4 v = rayMarch(ray);
    if (v.x < FarDistance) {
        vec3 iPoint = makePoint(ray, v.x);
        //vec4 n = noised(iPoint);
        vec3 normal = normalize(iPoint - planetOrigo);
        //vec3 normal = normalize(n.yzw);
        vec3 lightDir = normalize(LightSource - iPoint);

        float diffuse = max(0.0, dot(normal, lightDir));

        //color = vec3((n.x + 1.0) * 0.5);
        //color = normal;
        color = vec3(diffuse);
    }

    gl_FragColor = vec4(color, 1.0);
}

// From IQ.
float hash1(float n)
{
    return fract(n * 17.0 * fract(n * 0.3183099));
}

// From IQ.
vec4 noised(vec3 x)
{
    vec3 p = floor(x);
    vec3 w = fract(x);
    
    vec3 u = w*w*w*(w*(w*6.0-15.0)+10.0);
    vec3 du = 30.0*w*w*(w*(w-2.0)+1.0);

    float n = p.x + 317.0*p.y + 157.0*p.z;
    
    float a = hash1(n+0.0);
    float b = hash1(n+1.0);
    float c = hash1(n+317.0);
    float d = hash1(n+318.0);
    float e = hash1(n+157.0);
	float f = hash1(n+158.0);
    float g = hash1(n+474.0);
    float h = hash1(n+475.0);

    float k0 =   a;
    float k1 =   b - a;
    float k2 =   c - a;
    float k3 =   e - a;
    float k4 =   a - b - c + d;
    float k5 =   a - c - e + g;
    float k6 =   a - b - e + f;
    float k7 = - a + b + c - d + e - f - g + h;

    return vec4( -1.0+2.0*(k0 + k1*u.x + k2*u.y + k3*u.z + k4*u.x*u.y + k5*u.y*u.z + k6*u.z*u.x + k7*u.x*u.y*u.z), 
                      2.0* du * vec3( k1 + k4*u.y + k6*u.z + k7*u.y*u.z,
                                      k2 + k5*u.z + k4*u.x + k7*u.z*u.x,
                                      k3 + k6*u.x + k5*u.y + k7*u.x*u.y ) );
}

    |]