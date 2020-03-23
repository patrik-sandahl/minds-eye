module Subscriptions exposing (sub)

import Browser.Events as Events
import Data exposing (DragState(..), Model, MouseButton(..), Msg(..), Key (..))
import Json.Decode as Decode
import Viewport


{-| Application subscriptions.
-}
sub : Model -> Sub Msg
sub model =
    let
        staticEvents =
            [ Events.onResize
                (\w h ->
                    Viewport.init (toFloat w) (toFloat h)
                        |> ChangeViewport
                )
            , Events.onAnimationFrameDelta AnimateFrame
            , Events.onMouseDown (Decode.map3 MouseDown decodeMouseButton decodeMouseXPos decodeMouseYPos)
            , Events.onMouseUp (Decode.map MouseUp decodeMouseButton)
            , Events.onKeyDown (Decode.map KeyDown decodeKey)
            , Events.onKeyUp (Decode.map KeyUp decodeKey)
            , Events.onVisibilityChange
                (\v ->
                    if v == Events.Hidden then
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
            Events.onMouseMove (Decode.map2 MouseMoveTo decodeMouseXPos decodeMouseYPos)
                :: staticEvents
                |> Sub.batch


decodeMouseButton : Decode.Decoder MouseButton
decodeMouseButton =
    Decode.map
        (\v ->
            case v of
                0 ->
                    Left

                _ ->
                    OtherButton
        )
        (Decode.field "button" Decode.int)


decodeMouseXPos : Decode.Decoder Float
decodeMouseXPos =
    Decode.field "pageX" Decode.float


decodeMouseYPos : Decode.Decoder Float
decodeMouseYPos =
    Decode.field "pageY" Decode.float

decodeKey : Decode.Decoder Key
decodeKey =
    Decode.map 
        (\str ->
            case str of
                "Control" -> RotateKey
                "h"       -> HudToggleKey
                _         -> OtherKey
        ) (Decode.field "key" Decode.string)
