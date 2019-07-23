-- | Filter for hiding code blocks with attribute `.hidden`
module Main(main) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk(walk)
import Text.Pandoc.JSON(toJSONFilter)

removeHidden :: Block -> Block
removeHidden cb@(CodeBlock attrs@(_identifier, classes, kvs) str) =
  if "hidden" `elem` classes
     then Null
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
