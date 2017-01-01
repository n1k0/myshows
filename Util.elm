module Util exposing (stripTags)

import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)


stripTags : String -> String
stripTags html =
    parse html |> textContent
