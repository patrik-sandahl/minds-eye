module Viewport exposing
    ( Viewport
    , init
    , resolution
    )

import Math.Vector2 as V2 exposing (Vec2)


{-| Viewport with a width and height in given pixels.
-}
type alias Viewport =
    { width : Float
    , height : Float
    }


{-| Initialize the viewport.
-}
init : Float -> Float -> Viewport
init width height =
    { width = width
    , height = height
    }


{-| Get the viewport resolution as a Vec2, feasible for usage in shader program.
-}
resolution : Viewport -> Vec2
resolution viewport =
    V2.vec2 viewport.width viewport.height
