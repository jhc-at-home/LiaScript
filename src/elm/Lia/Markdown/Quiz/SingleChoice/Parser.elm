module Lia.Markdown.Quiz.SingleChoice.Parser exposing (parse)

import Combine
    exposing
        ( Parser
        , andMap
        , ignore
        , keep
        , many
        , map
        , string
        )
import Lia.Markdown.Inline.Parser exposing (line)
import Lia.Markdown.Inline.Types exposing (Inlines)
import Lia.Markdown.Quiz.SingleChoice.Types exposing (Quiz)
import Lia.Parser.Context exposing (Context)
import Lia.Parser.Helper exposing (newline, spaces)


parse : Parser Context Quiz
parse =
    map toQuiz unchecked
        |> andMap checked
        |> andMap unchecked


toQuiz : List Inlines -> Inlines -> List Inlines -> Quiz
toQuiz wrong1 right wrong2 =
    wrong1
        |> List.length
        |> Quiz (right :: wrong2 |> List.append wrong1)


checked : Parser Context Inlines
checked =
    spaces
        |> keep (string "[(X)]")
        |> keep line
        |> ignore newline


unchecked : Parser Context (List Inlines)
unchecked =
    spaces
        |> keep (string "[( )]")
        |> keep line
        |> ignore newline
        |> many