module XML.Query
where

import XML.Query.Prelude hiding (Text)
import qualified XML.Query.Prelude as Prelude
import qualified Success.Pure as Success


-- * Text
-------------------------

data Text a

-- |
-- Lifts an arbitrary textual parser function to a text-value parser.
-- 
-- Allows to compose with such libraries as \"parsec\" or \"attoparsec\".
text :: (Prelude.Text -> Either Prelude.Text a) -> Text a
text fn =
  undefined

-- |
-- Simply extracts the textual value.
textValue :: Text Prelude.Text
textValue =
  undefined


-- * Attr 
-------------------------

data Attr a

-- |
-- Lifts a text parser of the name to the attribute parser.
attrNameText :: Text a -> Attr a
attrNameText =
  undefined

attrValueText :: Text a -> Attr a
attrValueText =
  undefined


-- * Attrs
-------------------------

data Attrs a


-- * Tag
-------------------------

data Tag a

tagNameText :: Text a -> Tag a
tagNameText =
  undefined
-- tagAttr :: Attr a -> Tag a
-- tagContentText :: Text a -> Tag a
-- tagContentTag :: Tag a -> Tag a


-- ** Derivatives
-------------------------

tagNameIs :: Prelude.Text -> Tag ()
tagNameIs expected =
  tagNameText (text textParserFn)
  where
    textParserFn actual =
      if actual == expected
        then Right ()
        else Left ("tagNameIs: The actual name \"" <> actual <> "\" does not equal the expected \"" <> expected <> "\"")


-- * Node
-------------------------

data Node a

nodeTag :: Tag a -> Node a
nodeTag =
  undefined

nodeText :: Text a -> Node a
nodeText =
  undefined

nodeSpace :: Text a -> Node a
nodeSpace =
  undefined


-- * SeqNodes
-------------------------

data SeqNodes a

parNodes :: ParNodes a -> SeqNodes a
parNodes =
  undefined


-- * ParNodes
-------------------------

-- |
-- A parser for order-agnostic composition.
-- Searches thru a list of nodes for the first match,
-- while allowing the parallel composition.
data ParNodes a


