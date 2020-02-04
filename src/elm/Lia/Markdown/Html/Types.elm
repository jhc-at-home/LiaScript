module Lia.Markdown.Html.Types exposing (HtmlNode(..))

import Html.Parser exposing (Attribute)

type HtmlNode a
    = Text a
    | Element String (List Attribute) (List (HtmlNode a))
    | HtmlComment
