module App exposing (main)

import Browser.Hash as Hashbang
import Common.Types exposing (..)
import State
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
