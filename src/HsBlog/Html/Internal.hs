module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html =
  Html String

-- Type alias without constructor, not a real different type like newtype
type Title =
  String

newtype Structure =
  Structure String

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
  mempty = Structure ""

empty_ :: Structure
empty_ =
  Structure ""

el :: String -> (String -> String)
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

h_ :: Natural -> String -> Structure
h_ nr title =
  (Structure . el ("h" <> show nr) . escape) title

p_ :: String -> Structure
p_  =
  Structure . el "p" . escape

code_ :: String -> Structure
code_  =
  Structure . el "pre" . escape

genericList :: String -> String -> [Structure] -> Structure
genericList outerTag innerTag =
  Structure . el outerTag . concat . map (el innerTag . getStructureString)

ul_ :: [Structure] -> Structure
ul_ =
  genericList "ul" "li"

ol_ :: [Structure] -> Structure
ol_ =
  genericList "ul" "li"

html_ :: String -> Structure -> Html
html_ title content = Html (el "html" (el "head" ((el "title" (escape title)) <> (el "body" (getStructureString content)))))

render :: Html -> String
render html =
  case html of
    Html str -> str

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

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
