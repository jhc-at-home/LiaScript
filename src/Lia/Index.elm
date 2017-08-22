module Lia.Index exposing (create, scan)

import Lia.Inline.Type exposing (Inline(..), Reference(..))
import Lia.Quiz.Type exposing (Quiz(..), QuizBlock)
import Lia.Type exposing (Block(..), Slide)
import String


create : List Slide -> List String
create slides =
    List.map extract_string slides


scan : List String -> String -> List Int
scan index pattern =
    index
        |> List.indexedMap (,)
        |> List.filter (\( _, str ) -> String.contains (String.toLower pattern) str)
        |> List.map (\( i, _ ) -> i)


extract_string : Slide -> String
extract_string slide =
    slide.title
        ++ (slide.body
                |> List.map parse_block
                |> String.concat
           )
        |> String.toLower


parse_block : Block -> String
parse_block element =
    let
        scan e =
            List.map parse_inline e
                |> String.concat
    in
    case element of
        Paragraph e ->
            scan e

        Quote e ->
            scan e

        CodeBlock language code ->
            code

        Quiz block ->
            parse_quiz block.quiz

        EBlock _ sub_blocks ->
            List.map (\sub -> parse_block sub) sub_blocks
                |> String.concat

        _ ->
            ""


parse_quiz : Quiz -> String
parse_quiz quiz =
    case quiz of
        TextInput _ ->
            ""

        SingleChoice _ e ->
            List.map parse_inlines e
                |> String.concat

        MultipleChoice e ->
            List.map (\( _, ee ) -> parse_inlines ee) e
                |> String.concat


parse_inlines : List Inline -> String
parse_inlines list =
    List.map parse_inline list
        |> String.concat


parse_inline : Inline -> String
parse_inline element =
    case element of
        Chars str ->
            str

        Code str ->
            str

        Bold e ->
            parse_inline e

        Italic e ->
            parse_inline e

        Underline e ->
            parse_inline e

        Superscript e ->
            parse_inline e

        Ref e ->
            case e of
                Link alt_ url_ ->
                    alt_ ++ "" ++ url_

                Image alt_ url_ ->
                    alt_ ++ "" ++ url_

                Movie alt_ url_ ->
                    alt_ ++ "" ++ url_

        Formula _ str ->
            str

        HTML str ->
            str

        EInline _ e ->
            List.map parse_inline e
                |> String.concat

        _ ->
            ""