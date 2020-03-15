module View exposing (root)

import CommonTypes exposing (..)
import Browser
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Helpers.Element as EH
import Types exposing (..)


root : Model -> Browser.Document Msg
root model =
    Debug.todo ""