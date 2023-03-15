module Markup
  ( Document
  , Structure(..)
  )
where

import Numeric

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

parse :: String -> Document
parse = parseLines Nothing . lines -- Das Lines Argument wird einfach ausgelassen -> keine Variable benötigt

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    -- Paragraph case
    currentLine : rest
      let
        line = trim currentLine
      in
        if line = ""
           then maybe id (:) context (parseLines Nothing rest)
        else
          case context of
            Just (Paragraph paragraph) ->
              parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
            _ ->
              maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words


