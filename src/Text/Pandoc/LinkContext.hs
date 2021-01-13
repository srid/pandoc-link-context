module Text.Pandoc.LinkContext (queryLinksWithContext) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Block, Inline (Link), Pandoc (..))
import qualified Text.Pandoc.Walk as W

type Url = Text

-- | Attributes other than id and class
type OtherAttr = (Text, Text)

-- | Query the pandoc document for all links
--
-- Return a map, containing the "surrounding context" (as Pandoc blocks) for
-- each link.
queryLinksWithContext :: Pandoc -> Map Url ([OtherAttr], [Block])
queryLinksWithContext =
  fmap (second nub)
    . Map.fromListWith combineContext
    . W.query go
  where
    go :: Block -> [(Url, ([OtherAttr], [Block]))]
    go blk =
      fmap (\(url, attr) -> (url, (attr, [blk]))) $ case blk of
        B.Para is ->
          queryLinkUrls is
        B.Plain is ->
          queryLinkUrls is
        B.LineBlock is ->
          queryLinkUrls is
        B.Header _ _ is ->
          queryLinkUrls is
        B.DefinitionList xs ->
          -- Gather all filenames linked, and have them put (see above) in the
          -- same definition list block.
          concat $
            xs <&> \(is, bss) ->
              let def = queryLinkUrls is
                  body = fmap (fmap (fmap fst . go)) bss
               in def <> fmap (,[]) (concat (concat body))
        _ -> mempty

    queryLinkUrls :: W.Walkable Inline b => b -> [(Url, [OtherAttr])]
    queryLinkUrls =
      W.query (maybeToList . getLinkUrl)

    getLinkUrl :: Inline -> Maybe (Url, [OtherAttr])
    getLinkUrl = \case
      Link (_, _, attrs) _inlines (url, title) -> do
        -- Put title in attrs, as it *is* an attribute
        pure (url, ("title", title) : attrs)
      _ ->
        Nothing

    combineContext :: ([OtherAttr], [Block]) -> ([OtherAttr], [Block]) -> ([OtherAttr], [Block])
    combineContext (a1, b1) (a2, b2) =
      (a1 <> a2, b1 <> b2)
