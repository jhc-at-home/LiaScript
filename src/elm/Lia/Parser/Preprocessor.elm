module Lia.Parser.Preprocessor exposing (section)

import Combine
    exposing
        ( Parser (..)
        , andMap
        , choice
        , fail
        , ignore
        , keep
        , many
        , map
        , regex
        , string
        , succeed
        , withColumn
        )
import Lia.Markdown.Inline.Parser exposing (line)
import Lia.Markdown.Inline.Types exposing (Inlines)
import Lia.Parser.Context exposing (Context)
import Lia.Parser.Helper exposing (newline)
import Lia.Section as Section

import Debug exposing (log)


title_tag : Parser Context Int
title_tag =
    regex "#+" |> map String.length


check : Int -> Parser s ()
check c =
    if c /= 0 then
        succeed ()

    else
        fail ""


title_str : Parser Context Inlines
title_str =
    line |> ignore newline


body : Parser Context String
body =
    let
     parsers = [ regex "(?:[^#`<]+|[\\x0D\n]+|<!--[\\S\\s]*?-->)" -- comment
               , regex "(`{3,})[\\S\\s]*?\\1" -- code_block or ascii art
               , regex "`.+?`" -- code_block or ascii art
               , regex "(?:<([\\w+\\-]+)[\\S\\s]*?</\\2>|`|<)"
               , withColumn check |> keep (string "#")
               ]
     bodyParser : Parser Context String
     bodyParser =      parsers
         |> choice
         |> many
         |> map logParserVal
    in
        bodyParser |> log "bodyParser"
        -- case bodyParser of
        --     Parser (ParserFn ctx val) ->
        --         log val |> Parser ctx


logParserVal : List String -> String
logParserVal = String.concat >> log "Parser"


section : Parser Context Section.Base
section =
    title_tag
        |> map Section.Base
        |> andMap title_str
        |> andMap body
