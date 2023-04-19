module HsBlog
  ( convertSingle
  , convertDirectory
  , process
  , buildIndex
  , confirm
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)
import HsBlog.Directory (convertDirectory, buildIndex, confirm)
import HsBlog.Env (defaultEnv)

import System.IO


convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Html.Title -> String -> String
process title =
  Html.render . convert defaultEnv title . Markup.parse

