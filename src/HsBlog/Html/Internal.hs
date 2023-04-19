module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html =
  Html String

-- Type alias without constructor, not a real different type like newtype
type Title =
  String

-- Html <head>
newtype Head
  = Head String

newtype Structure =
  Structure String

newtype Content =
  Content String

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
  mempty = Structure ""

instance Semigroup Head where
  (<>) (Head h1) (Head h2) =
    Head (h1 <> h2)

instance Monoid Head where
  mempty = Head ""

html_ :: Head -> Structure -> Html
html_ (Head head) content =
  Html
    (el "html"
      (el "head" head
        <> el "body" (getStructureString content)))

stylesheet_ :: FilePath -> Head
stylesheet_ path =
  Head $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

meta_ :: String -> String -> Head
meta_ name content =
  Head $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

title_ :: String -> Head
title_ = Head . el "title" . escape

h_ :: Natural -> Content -> Structure
h_ nr title =
  (Structure . el ("h" <> show nr) . getContentString) title

p_ :: Content -> Structure
p_  =
  Structure . el "p" . getContentString

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


-- * Content

txt_ :: String -> Content
txt_ =
  Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href?=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\""

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)

instance Semigroup Content where
  (<>) c1 c2 =
    Content (getContentString c1 <> getContentString c2)

instance Monoid Content where
  mempty = Content ""


-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utils

el :: String -> (String -> String)
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

getContentString :: Content -> String
getContentString content =
  case content of
    Content str -> str

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
