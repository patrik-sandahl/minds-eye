module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Camera exposing (Camera)
import Cs
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Json.Decode as Decode
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Navigator exposing (Mode(..), Navigator)
import Ray exposing (Ray)
import Sphere exposing (Sphere)
import Task
import Viewport exposing (Viewport)
import WebGL exposing (Mesh, Shader)


type DragState
    = Static
    | Dragging


type alias Model =
    { viewport : Viewport
    , latestFrameTimes : List Float
    , playTime : Float
    , dragState : DragState
    , navigator : Navigator
    , quadMesh : Mesh Vertex
    , planet : Sphere
    }


type MouseButton
    = Left
    | Other
    | Any


type Msg
    = ChangeViewport Viewport
    | AnimateFrame Float
    | MouseDown MouseButton Float Float
    | MouseMoveTo Float Float
    | MouseUp MouseButton
    | Ignore


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        planet =
            Sphere.init (V3.vec3 0.0 0.0 0.0) 1.0
    in
    ( { viewport = Viewport.init 0 0
      , latestFrameTimes = []
      , playTime = 0.0
      , dragState = Static
      , navigator = Orbit planet |> Navigator.init
      , quadMesh = makeQuadMesh
      , planet = planet
      }
    , fetchResolution
    )


fetchResolution : Cmd Msg
fetchResolution =
    Task.perform
        (\viewport ->
            Viewport.init viewport.viewport.width viewport.viewport.height |> ChangeViewport
        )
        Dom.getViewport


view : Model -> Html Msg
view model =
    Html.div
        []
        [ viewHud model
        , WebGL.toHtmlWith
            [ WebGL.antialias
            ]
            [ model.viewport.width |> HtmlAttributes.width << floor
            , model.viewport.height |> HtmlAttributes.height << floor
            , HtmlAttributes.style "display" "block"
            ]
            [ WebGL.entity
                quadVertexShader
                playgroundFragmentShader
                model.quadMesh
                { resolution = Viewport.resolution model.viewport
                , playTime = model.playTime
                , planetOrigo = model.planet.origo
                , planetRadius = model.planet.radius
                , cameraEye = model.navigator.camera.eye
                , cameraForward = model.navigator.camera.forward
                , cameraRight = model.navigator.camera.right
                , cameraUp = model.navigator.camera.up
                , cameraFocalLength = model.navigator.camera.focalLength
                }
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeViewport viewport ->
            ( { model
                | viewport = viewport
              }
            , Cmd.none
            )

        AnimateFrame delta ->
            ( { model
                | latestFrameTimes = delta :: List.take 4 model.latestFrameTimes
                , playTime = model.playTime + delta
                , navigator = Navigator.tick ((pi * 0.1) / 60) model.navigator
              }
            , Cmd.none
            )

        MouseDown Left pageX pageY ->
            let
                dbg =
                    Debug.log ("Down: x=" ++ String.fromFloat pageX ++ ", y=" ++ String.fromFloat pageY) 0
            in
            ( { model
                | dragState = Dragging
              }
            , Cmd.none
            )

        MouseDown _ _ _ ->
            ( model, Cmd.none
            )

        MouseMoveTo pageX pageY ->
            let
                dbg =
                    Debug.log ("MoveTo: x=" ++ String.fromFloat pageX ++ ", y=" ++ String.fromFloat pageY) 0
            in
            ( model
            , Cmd.none
            )

        MouseUp _ ->
            let
                dbg =
                    Debug.log "Up" 0
            in
            ( { model
                | dragState = Static
              }
            , Cmd.none
            )

        Ignore ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        staticEvents =
            [ BrowserEvents.onResize
                (\w h ->
                    Viewport.init (toFloat w) (toFloat h)
                        |> ChangeViewport
                )
            , BrowserEvents.onAnimationFrameDelta AnimateFrame
            , BrowserEvents.onMouseDown (Decode.map3 MouseDown decodeMouseButton decodeMouseXPos decodeMouseYPos)
            , BrowserEvents.onMouseUp (Decode.map MouseUp decodeMouseButton)
            , BrowserEvents.onVisibilityChange
                (\v ->
                    if v == BrowserEvents.Hidden then
                        MouseUp Any

                    else
                        Ignore
                )
            ]
    in
    case model.dragState of
        Static ->
            Sub.batch staticEvents

        Dragging ->
            BrowserEvents.onMouseMove (Decode.map2 MouseMoveTo decodeMouseXPos decodeMouseYPos) :: staticEvents |> Sub.batch


decodeMouseButton : Decode.Decoder MouseButton
decodeMouseButton =
    Decode.map
        (\v ->
            case v of
                0 ->
                    Left

                _ ->
                    Other                
        )
        (Decode.field "button" Decode.int)


decodeMouseXPos : Decode.Decoder Float
decodeMouseXPos =
    Decode.field "pageX" Decode.float


decodeMouseYPos : Decode.Decoder Float
decodeMouseYPos =
    Decode.field "pageY" Decode.float


calcFps : List Float -> Float
calcFps latestFrameTimes =
    if not (List.isEmpty latestFrameTimes) then
        let
            avg =
                List.sum latestFrameTimes / toFloat (List.length latestFrameTimes)
        in
        if avg /= 0.0 then
            1000.0 / avg

        else
            0.0

    else
        0.0

intersectPlanet : Vec2 -> Model -> Maybe Vec3
intersectPlanet uv model = 
    let ray = Camera.uvToRay uv model.navigator.camera
        d = Sphere.intersect ray model.planet
    in 
        case d of
            Just dd -> Ray.pointAt dd ray |> Just
            Nothing -> Nothing

viewHud : Model -> Html Msg
viewHud model =
    Html.div
        [ HtmlAttributes.style "display" "inline-block"
        , HtmlAttributes.style "position" "fixed"
        , HtmlAttributes.style "left" "10px"
        , HtmlAttributes.style "top" "10px"
        , HtmlAttributes.style "font-family" "sans-serif"
        , HtmlAttributes.style "font-size" "16px"
        , HtmlAttributes.style "color" "white"
        , HtmlAttributes.style "z-index" "1"
        ]
        [ let
            res =
                String.fromFloat model.viewport.width ++ "x" ++ String.fromFloat model.viewport.height ++ "px"

            fps =
                String.fromInt (calcFps model.latestFrameTimes |> round) ++ " FPS"

            eye =
                model.navigator.camera.eye

            eyePos =
                "Eye x="
                    ++ (V3.getX eye |> String.fromFloat)
                    ++ ", y="
                    ++ (V3.getY eye |> String.fromFloat)
                    ++ ", z="
                    ++ (V3.getZ eye |> String.fromFloat)
          in
          res ++ " " ++ fps ++ " " ++ eyePos |> Html.text
        ]


type alias Vertex =
    { position : Vec3
    }


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


makeQuadMesh : Mesh Vertex
makeQuadMesh =
    WebGL.triangleStrip
        [ Vertex (V3.vec3 -1.0 1.0 0.0)
        , Vertex (V3.vec3 -1.0 -1.0 0.0)
        , Vertex (V3.vec3 1.0 1.0 0.0)
        , Vertex (V3.vec3 1.0 -1.0 0.0)
        ]


quadVertexShader : Shader Vertex Uniforms {}
quadVertexShader =
    [glsl|

precision highp float;

attribute vec3 position;

void main()
{
    gl_Position = vec4(position, 1.0);
}

    |]


playgroundFragmentShader : Shader {} Uniforms {}
playgroundFragmentShader =
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
