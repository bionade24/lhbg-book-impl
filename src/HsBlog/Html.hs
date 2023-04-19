module HsBlog.Html
  ( Html
  , Title
  , Structure
  , html_
  , meta_
  , stylesheet_
  , title_
  , h_
  , p_
  , ul_
  , ol_
  , code_
  , Content
  , txt_
  , img_
  , link_
  , b_
  , i_
  , render
  )
  where

import HsBlog.Html.Internal
import GHC.IO.Handle.Internals (hLookAhead_)

