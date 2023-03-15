module Convert where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import qualified Markup
import qualified Html
import qualified Html.Internal as HI


convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading nr txt ->
      Html.h_ nr txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnroderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
