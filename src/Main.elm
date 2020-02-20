module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Task
import WebGL exposing (Mesh, Shader)


type alias Model =
    { viewportWidth : Float
    , viewportHeight : Float
    , playTime : Float
    }


type Msg
    = ResizeViewport Float Float
    | AnimateFrame Float


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
      , playTime = 0.0
      }
    , fetchViewportSize
    )


fetchViewportSize : Cmd Msg
fetchViewportSize =
    Task.perform (\viewport -> ResizeViewport viewport.viewport.width viewport.viewport.height) Dom.getViewport


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
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
            }
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeViewport w h ->
            ( { model | viewportWidth = w, viewportHeight = h }
            , Cmd.none
            )

        AnimateFrame delta ->
            ( { model | playTime = model.playTime + delta }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BrowserEvents.onResize (\w h -> ResizeViewport (toFloat w) (toFloat h))
        , BrowserEvents.onAnimationFrameDelta AnimateFrame
        ]


type alias Vertex =
    { position : Vec3
    }


type alias Uniforms =
    { resolution : Vec2
    , playTime : Float
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

vec2 normalizedUV()
{
    return (gl_FragCoord.xy - 0.5 * resolution) / min(resolution.x, resolution.y);
}

void main()
{
    vec2 uv = fract(normalizedUV() * 10.0);
    gl_FragColor = vec4(uv.x, uv.y, 0.0, 1.0);
}

    |]
