module Topic exposing (Topic)

import Element


type Topic
    = Topic String


display : Topic -> Element msg
display t =
    case t of
        Topic str ->
            Element.text str
