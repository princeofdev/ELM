module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element exposing (Element, FocusStyle, focusStyle)


type alias View msg =
    { title : String
    , attributes : List (Element.Attribute msg)
    , element : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , attributes = []
    , element = Element.text str
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , attributes = view.attributes |> List.map (Element.mapAttribute fn)
    , element = Element.map fn view.element
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ Element.layoutWith { options = [ focusStyle (FocusStyle Nothing Nothing Nothing) ] } view.attributes view.element
        ]
    }
