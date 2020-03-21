module Main exposing (main)

import Browser
import Data exposing (Model, Msg)
import Subscriptions
import Update
import View

{-| Application main function. -}
main : Program () Model Msg
main =
    Browser.element
        { init = Update.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.sub
        }
