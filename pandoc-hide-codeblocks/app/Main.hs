-- | Filter for hiding code blocks with attribute `.hidden`
module Main(main) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk(walk)
import Text.Pandoc.JSON(toJSONFilter)
import Debug.Trace

removeHidden :: Block -> Block
removeHidden cb@(CodeBlock attrs@(_identifier, classes, kvs) str) =
  if "hidden" `elem` classes
     then Null
     else if "haskell" `notElem` classes
            then trace ("Unknown class: " <> show classes <> "\nin block: " <> show cb) cb -- FIXME: ugly imperative debug
            else cb
removeHidden other                                                = other

{-
readDoc s = case readMarkdown def s of
              Right doc -> doc
              Left  err -> error $ show err

writeDoc doc = writeMarkdown def doc
 -}

main = toJSONFilter removeHidden
-- interact $ writeDoc . walk removeHidden . readDoc
