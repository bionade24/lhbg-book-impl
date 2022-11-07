module Html.Internal where

newtype Structure = Structure String
newtype Html = Html String
type Title = String

getStructureString (Structure str) = str

append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure (s1 <> s2)

el :: String -> (String -> String)
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

h1_  = Structure . el "h1" . escape
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





