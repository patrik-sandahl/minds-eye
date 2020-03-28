module View exposing (view)

import Data exposing (Model, Msg)
import Html exposing (Html)
import Html.Attributes as Attr
import Math.Vector3 as V3
import Pipeline exposing (Pipe(..))


{-| The application view function.
-}
view : Model -> Html Msg
view model =
    Html.div
        []
        [ viewHud model
        , Pipeline.view model.pipe model.viewport model.navigator.camera model.planet model.playTime model.pipeline
        ]


viewHud : Model -> Html Msg
viewHud model =
    Html.div
        [ Attr.style "display" "inline-block"
        , Attr.style "position" "fixed"
        , Attr.style "left" "10px"
        , Attr.style "top" "10px"
        , Attr.style "font-family" "sans-serif"
        , Attr.style "font-size" "16px"
        , Attr.style "color" "white"
        , Attr.style "z-index" "1"
        , Attr.style "visibility" <|
            if model.showHud then
                "visible"

            else
                "hidden"
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
