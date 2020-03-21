module Data exposing
    ( DragState(..)
    , Model
    , MouseButton(..)
    , Msg(..)
    )

import Navigator exposing (Navigator)
import Pipeline exposing (Pipeline)
import Sphere exposing (Sphere)
import Viewport exposing (Viewport)


{-| State telling if the user is dragging the mouse.
-}
type DragState
    = Static
    | Dragging


{-| Application main model.
-}
type alias Model =
    { viewport : Viewport
    , latestFrameTimes : List Float
    , playTime : Float
    , dragState : DragState
    , navigator : Navigator
    , pipeline : Pipeline
    , planet : Sphere
    }


{-| Mouse button enumeration.
-}
type MouseButton
    = Left
    | Other
    | Any


{-| Application message type.
-}
type Msg
    = ChangeViewport Viewport
    | AnimateFrame Float
    | MouseDown MouseButton Float Float
    | MouseMoveTo Float Float
    | MouseUp MouseButton
    | Ignore
