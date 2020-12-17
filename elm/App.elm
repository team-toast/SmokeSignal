module App exposing (main)

import Browser.Hashbang as Hashbang
import State
import Types exposing (..)
import View


main : Program Flags Model Msg
main =
    Hashbang.application
        { init = State.init
        , view = View.root
        , update = State.update
        , subscriptions = State.subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
