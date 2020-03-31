module Data exposing
    ( DragState(..)
    , Key(..)
    , Model
    , MouseButton(..)
    , Msg(..)
    )

import Math.Sphere exposing (Sphere)
import Navigator exposing (Navigator)
import Pipeline exposing (Pipe, Pipeline)
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
    , showHud : Bool
    , rotating : Bool
    , pipe : Pipe
    }


{-| Mouse button enumeration.
-}
type MouseButton
    = Left
    | OtherButton
    | Any


{-| Key enumeration.
-}
type Key
    = RotateKey
    | HudToggleKey
    | Dev0Key
    | Dev1Key
    | OtherKey


{-| Application message type.
-}
type Msg
    = ChangeViewport Viewport
    | AnimateFrame Float
    | MouseDown MouseButton Float Float
    | MouseMoveTo Float Float
    | MouseUp MouseButton
    | KeyDown Key
    | KeyUp Key
    | Ignore
