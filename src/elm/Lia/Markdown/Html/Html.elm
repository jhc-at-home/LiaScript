module Lia.Markdown.Html.Html exposing (Attribute, attribute, attributes, Node(..), renderHtml, unify, tags)

import Combine exposing ( andMap
                        , choice
                        , ignore
                        , keep
                        , many
                        , map
                        , Parser
                        , regex
                        , regexWith
                        , string
                        , whitespace)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Parser
import Lia.Markdown.Html.Types exposing (HtmlNode(..))
import Lia.Parser.Helper exposing (stringTill)

unify : List Html.Parser.Node -> Html.Parser.Node
unify list =
    case list of
        [] ->
            Html.Parser.Comment ""

        [ ns ] ->
            ns

        ns ->
            Html.Parser.Element "div" [] ns
            

-- parseHtml : (List Node -> List (HtmlNode a))
--            -> String
--            -> List (HtmlNode a)
-- parseHtml convert str =
--     case run str of
--         Ok rslt ->
--             convert (Debug.log "HTML AST" rslt)

--         Err _ ->
--             []
parseHtml :  (List Html.Parser.Node -> List (HtmlNode a))
           -> String
           -> List (HtmlNode a)
parseHtml _ _ = []

renderHtml : (a -> Html msg) -> HtmlNode a -> Html msg
renderHtml view n =
    let
        renderAttr : (String, String) -> Html.Attribute msg
        renderAttr ( name, value ) =
            Attr.attribute name value
    in
        case n of
            Text a ->
                view a

            Element name attrs children ->
                Html.node name (List.map renderAttr attrs) (List.map (renderHtml view) children)

            HtmlComment ->
                Html.text ""

type alias Attribute = (String, String)
                    
type Node a
    = Node String (List Attribute) a -- first argument is the element name
    | Final String (List Attribute)

attribute : Parser s Attribute
attribute =
    whitespace
        |> keep (regex "\\w+")
        |> ignore (regex "[ \t\n]*=[ \t\n]*\"")
        |> map (\k v -> ( String.toLower k, v ))
        |> andMap (stringTill (regex "\"[ \t\n]*"))

attributes : Parser s (List Attribute)
attributes =
    many attribute

htmlOpenTag : Parser s String -> Parser s (a -> Node a)
htmlOpenTag tag =
    string "<"
        |> ignore whitespace
        |> keep tag
        |> ignore whitespace
        |> map Node
        |> andMap attributes
        |> ignore (string ">")

htmlCloseTag : Parser s String -> Parser s String
htmlCloseTag tag =
    string "</"
        |> keep tag
        |> ignore (string ">")

pairedElement : Parser s a -> String -> Parser s (Node a)
pairedElement body tag =
    let
        tagParser = regexWith True False tag
    in
        htmlOpenTag tagParser
            |> andMap body
            |> ignore (htmlCloseTag tagParser)

unpairedElement : String -> Parser s (Node a)
unpairedElement tag =
    let
        tagParser = regexWith True False tag
    in
        string "<"
            |> ignore whitespace
            |> keep tagParser
            |> ignore whitespace
            |> map Final
            |> andMap attributes
            |> ignore (regex "/?>")



tags : Parser s a -> Parser s (Node a)
tags body =
    let
        paired = List.map (pairedElement body) ["a"
                                               , "abbr"
                                               , "address"
                                               , "area"
                                               , "article"
                                               , "aside"
                                               , "audio"
                                               , "b"
                                               , "bdi"
                                               , "bdo"
                                               , "blockquote"
                                               , "body"
                                               , "button"
                                               , "canvas"
                                               , "caption"
                                               , "cite"
                                               , "code"
                                               , "colgroup"
                                               , "data"
                                               , "datalist"
                                               , "dd"
                                               , "del"
                                               , "details"
                                               , "dfn"
                                               , "dialog"
                                               , "div"
                                               , "dl"
                                               , "dt"
                                               , "em"
                                               , "embed"
                                               , "fieldset"
                                               , "figcaption"
                                               , "figure"
                                               , "footer"
                                               , "form"
                                               , "h[1-6]"
                                               , "head[e][r]"
                                               , "html"
                                               , "i"
                                               , "kbd"
                                               , "label"
                                               , "legend"
                                               , "li"
                                               , "main"
                                               , "map"
                                               , "mark"
                                               , "meter"
                                               , "nav"
                                               , "noscript"
                                               , "object"
                                               , "ol"
                                               , "optgroup"
                                               , "option"
                                               , "output"
                                               , "p"
                                               , "picture"
                                               , "pre"
                                               , "progress"
                                               , "q"
                                               , "rp"
                                               , "rt"
                                               , "ruby"
                                               , "s"
                                               , "samp"
                                               , "script"
                                               , "section"
                                               , "select"
                                               , "small"
                                               , "span"
                                               , "strong"
                                               , "style"
                                               , "sub"
                                               , "summary"
                                               , "sup"
                                               , "svg"
                                               , "table"
                                               , "tbody"
                                               , "td"
                                               , "template"
                                               , "tfoot"
                                               , "th"
                                               , "time"
                                               , "tr"
                                               , "u"
                                               , "ul"
                                               , "var"
                                               , "video"
                                               ]

        unpaired = List.map unpairedElement [ "base"
                                            , "br"
                                            , "canvas"
                                            , "col"
                                            , "hr"
                                            , "img"
                                            , "input"
                                            , "ins"
                                            , "link"
                                            , "meta"
                                            , "object"
                                            , "output"
                                            , "param"
                                            , "progress"
                                            , "source"
                                            , "track"
                                            , "wbr"
                                            ]
            
    in
        choice (List.append paired unpaired)
