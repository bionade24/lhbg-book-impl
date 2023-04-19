module HsBlog.Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural
import Data.Maybe (maybeToList)

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

trimWhitespace :: String -> String
trimWhitespace = unwords . words

parse :: String -> Document
parse = parseLines Nothing . lines -- Das Lines Argument wird einfach ausgelassen -> keine Variable benötigt

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- h1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trimWhitespace line) : parseLines Nothing rest)

    -- ul case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trimWhitespace line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trimWhitespace line])) rest)

    -- ol case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trimWhitespace line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trimWhitespace line])) rest)

    -- Codeblock case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          -- Codeblock gets appended with new codeblock content
          parseLines (Just (CodeBlock (code <> [line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [trimWhitespace line])) rest)

    -- Paragraph case
    currentLine : rest ->
      let
        line = trimWhitespace currentLine
      in
        if line == ""
           then maybe id (:) context (parseLines Nothing rest)
        else
          case context of
            Just (Paragraph paragraph) ->
              parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
            _ ->
              maybe id (:) context (parseLines (Just (Paragraph line)) rest)

