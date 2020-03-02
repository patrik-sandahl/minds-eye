module Viewport exposing
    ( Viewport
    , init
    , normalizedUV
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


{-| Calculate a normalized UV coordinate (where 0, 0 is in the center) from the xy argument.
-}
normalizedUV : Vec2 -> Viewport -> Vec2
normalizedUV xy viewport =
    let
        nd =
            min viewport.width viewport.height

        u =
            (V2.getX xy - 0.5 * viewport.width) / nd

        v =
            (V2.getY xy - 0.5 * viewport.height) / nd
    in
    V2.vec2 u v
