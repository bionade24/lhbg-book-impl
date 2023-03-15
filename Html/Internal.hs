module Html.Internal where

import Numeric

newtype Html = Html String
type Title = String
type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]

empty_ :: Structure
empty_ = Structure ""

instance Monioid Structure where
  mempty = empty_

el :: String -> (String -> String)
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

h_ :: Natural -> String -> Structure
h_ nr title =
  Structure . el ("h" <> show nr) . escape title
p_  = Structure . el "p" . escape
code_  = Structure . el "pre" . escape

list :: String -> String -> [Structure] -> Structure
list outerTag innerTag = Structure . el outerTag . concat . map (el innerTag . getStructureString)

ul_ = list "ul" "li"
ol_ = list "ul" "li"

html_ :: String -> Structure -> Html
html_ title content = Html (el "html" (el "head" ((el "title" (escape title)) <> (el "body" (getStructureString content)))))

render_ :: Html -> String
render_ html =
  case html of
    Html str -> str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
  in
    concat . map escapeChar

concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x : xs -> x <> concatStructure xs
