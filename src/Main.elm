module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Json.Decode as Decode
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Task
import WebGL exposing (Mesh, Shader)

type DragState 
    = Static
    | Dragging

type alias Model =
    { viewportWidth : Float
    , viewportHeight : Float
    , latestFrameTimes : List Float
    , playTime : Float
    , dragState : DragState
    }


type MouseButton
    = Left
    | Mid
    | Right
    | Any

type Msg
    = ResizeViewport Float Float
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
    ( { viewportWidth = 0.0
      , viewportHeight = 0.0
      , latestFrameTimes = []
      , playTime = 0.0
      , dragState = Static
      }
    , fetchViewportSize
    )


fetchViewportSize : Cmd Msg
fetchViewportSize =
    Task.perform (\viewport -> ResizeViewport viewport.viewport.width viewport.viewport.height) Dom.getViewport


view : Model -> Html Msg
view model =
    let
        camera =
            lookAt (V3.vec3 0.0 0.0 -5.0) (V3.vec3 0.0 0.0 0.0) (V3.vec3 0.0 1.0 0.0)
    in
    Html.div
        []
        [ viewHud model
        , WebGL.toHtmlWith
            [ WebGL.antialias
            ]
            [ floor model.viewportWidth |> HtmlAttributes.width
            , floor model.viewportHeight |> HtmlAttributes.height
            , HtmlAttributes.style "display" "block"
            ]
            [ WebGL.entity
                quadVertexShader
                playgroundFragmentShader
                quadMesh
                { resolution = V2.vec2 model.viewportWidth model.viewportHeight
                , playTime = model.playTime
                , eye = camera.eye
                , forward = camera.forward
                , right = camera.right
                , up = camera.up
                , focalLength = camera.focalLength
                }
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeViewport w h ->
            ( { model | viewportWidth = w, viewportHeight = h }
            , Cmd.none
            )

        AnimateFrame delta ->
            ( { model | latestFrameTimes = delta :: List.take 4 model.latestFrameTimes, playTime = model.playTime + delta }
            , Cmd.none
            )

        MouseDown button pageX pageY ->
            let dbg = Debug.log ("Down: x=" ++ String.fromFloat pageX ++ ", y=" ++ String.fromFloat pageY) 0
            in ( { model | dragState = Dragging }
            , Cmd.none
            )

        MouseMoveTo pageX pageY ->
            let dbg = Debug.log ("MoveTo: x=" ++ String.fromFloat pageX ++ ", y=" ++ String.fromFloat pageY) 0
            in ( model
            , Cmd.none
            )

        MouseUp button ->
            let dbg = Debug.log "Up" 0
            in ( { model | dragState = Static}
            , Cmd.none
            )

        Ignore ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let staticEvents = 
            [ BrowserEvents.onResize (\w h -> ResizeViewport (toFloat w) (toFloat h))
            , BrowserEvents.onAnimationFrameDelta AnimateFrame
            , BrowserEvents.onMouseDown (Decode.map3 MouseDown decodeMouseButton decodeMouseXPos decodeMouseYPos)
            , BrowserEvents.onMouseUp (Decode.map MouseUp decodeMouseButton)
            , BrowserEvents.onVisibilityChange (\v -> if v == BrowserEvents.Hidden then MouseUp Any else Ignore)
            ]
    in case model.dragState of
        Static -> Sub.batch staticEvents
        Dragging -> BrowserEvents.onMouseMove (Decode.map2 MouseMoveTo decodeMouseYPos decodeMouseXPos) :: staticEvents |> Sub.batch

decodeMouseButton : Decode.Decoder MouseButton
decodeMouseButton =
    Decode.map (\v ->
        case v of
            0 -> Left
            1 -> Mid
            _ -> Right
    ) (Decode.field "button" Decode.int)

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
                String.fromFloat model.viewportWidth ++ "x" ++ String.fromFloat model.viewportHeight ++ "px"

            fps =
                String.fromInt (calcFps model.latestFrameTimes |> round) ++ " FPS"
          in
          res ++ " " ++ fps |> Html.text
        ]


type alias Camera =
    { eye : Vec3
    , forward : Vec3
    , right : Vec3
    , up : Vec3
    , focalLength : Float
    }


lookAt : Vec3 -> Vec3 -> Vec3 -> Camera
lookAt eye at upDir =
    let
        forward =
            V3.sub at eye |> V3.normalize

        right =
            V3.cross forward upDir |> V3.normalize

        up =
            V3.cross right forward
    in
    { eye = eye, forward = forward, right = right, up = up, focalLength = 0.3 }


type alias Vertex =
    { position : Vec3
    }


type alias Uniforms =
    { resolution : Vec2
    , playTime : Float
    , eye : Vec3
    , forward : Vec3
    , right : Vec3
    , up : Vec3
    , focalLength : Float
    }


quadMesh : Mesh Vertex
quadMesh =
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

uniform vec3 eye;
uniform vec3 forward;
uniform vec3 right;
uniform vec3 up;
uniform float focalLength;

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
    vec3 center = eye + forward * focalLength;
    vec3 spot = center + right * uv.x + up * uv.y;

    return makeRay(eye, spot - eye);
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
    return sphere(p - vec3(0.0), 1.0);
}

float rayMarch(Ray ray)
{
    float d0 = 0.0;
    for (int i = 0; i < 100; ++i) {
        vec3 p = makePoint(ray, d0);
        float d = intersectScene(p);

        d0 += d;
        if (d < 0.001 || d0 > 10.0) break;
    }

    return d0;
}

void main()
{
    //vec2 uv = fract(normalizedUV() * 10.0);
    vec2 uv = normalizedUV();
    Ray ray = primaryRay(uv);

    float d = rayMarch(ray);
    vec3 color = d < 10.0 ? vec3(1.0) : vec3(0.3);

    gl_FragColor = vec4(color, 1.0);

    //gl_FragColor = vec4(uv.x, uv.y, 0.0, 1.0);
    //gl_FragColor = vec4(ray.direction, 1.0);
}

    |]