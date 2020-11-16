module Text.Pandoc.LinkContext (queryLinksWithContext) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Attr, Block, Inline (Link), Pandoc (..), Target)
import qualified Text.Pandoc.Walk as W

type Url = Text

queryLinksWithContext :: Pandoc -> Map Url [Block]
queryLinksWithContext =
  fmap nub
    . Map.fromListWith (<>)
    . fmap (second one)
    . W.query go
  where
    go :: Block -> [(Url, Block)]
    go blk =
      fmap (,blk) $ case blk of
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
            flip fmap xs $ \(is, bss) ->
              let def = queryLinkUrls is
                  body = fmap (fmap (fmap fst . go)) bss
               in def <> concat (concat body)
        _ -> mempty

    queryLinkUrls :: W.Walkable Inline b => b -> [Url]
    queryLinkUrls =
      W.query (maybeToList . getLinkUrl)

    getLinkUrl :: Inline -> Maybe Url
    getLinkUrl x = do
      (_attr, _inlines, (url, _title)) <- getLink x
      pure url

    getLink :: Inline -> Maybe (Attr, [Inline], Target)
    getLink = \case
      Link attr inlines target@(url, _title) -> do
        guard $ not $ "/" `T.isInfixOf` url
        guard $ ".md" `T.isSuffixOf` url
        pure (attr, inlines, target)
      _ ->
        Nothing
