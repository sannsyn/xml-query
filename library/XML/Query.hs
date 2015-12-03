module XML.Query
where

import XML.Query.Prelude hiding (Text)
import qualified XML.Query.Prelude as Prelude
import qualified XML.Query.AST as AST


-- * Text
-------------------------

-- |
-- Parser in the context of a textual value.
type Text =
  Alt AST.Text

-- |
-- Lifts an arbitrary textual parser function to the text-value parser.
-- 
-- Provides a doorway for composition with such libraries as \"parsec\" or \"attoparsec\".
text :: (Prelude.Text -> Either Prelude.Text a) -> Text a
text =
  liftAlt . AST.Text

-- ** Derivatives
-------------------------

-- |
-- Simply extracts the textual value.
textValue :: Text Prelude.Text
textValue =
  text pure


-- * Tag
-------------------------

type Tag =
  Alt AST.Tag

tagNameText :: Text a -> Tag a
tagNameText =
  liftAlt . AST.TagNameText

-- |
-- Parses one of tag's attributes without any regard to order.
tagAttr :: Attr a -> Tag a
tagAttr =
  liftAlt . AST.TagAttr

-- |
-- Parses all of tag's nodes.
-- 
-- Can be used multiple times,
-- thus allowing for parallel parsing of tag's child-nodes.
-- Naturally this will result in traversing the tag's nodes multiple times.
tagNodes :: Nodes a -> Tag a
tagNodes =
  liftAlt . AST.TagNodes

-- |
-- Parses a single node,
-- located anywhere among the tag's children.
tagNode :: Node a -> Tag a
tagNode =
  liftAlt . AST.TagNode

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


-- * Attr
-------------------------

type Attr =
  Alt AST.Attr

-- |
-- Parses the attribute's name using the provided textual parser.
attrNameText :: Text a -> Attr a
attrNameText =
  liftAlt . AST.AttrNameText

-- |
-- Parses the attribute's value using the provided textual parser.
attrValueText :: Text a -> Attr a
attrValueText =
  liftAlt . AST.AttrValueText

-- ** Derivatives
-------------------------

-- |
-- A parser, which succeeds if the attribute's name matches the provided value.
attrNameIs :: Prelude.Text -> Attr ()
attrNameIs expected =
  attrNameText (text textParserFn)
  where
    textParserFn actual =
      if actual == expected
        then Right ()
        else Left ("attrNameIs: The actual name \"" <> actual <> "\" does not equal the expected \"" <> expected <> "\"")

-- |
-- A parser, which succeeds if the attribute's value matches the provided value.
attrValueIs :: Prelude.Text -> Attr ()
attrValueIs expected =
  attrValueText (text textParserFn)
  where
    textParserFn actual =
      if actual == expected
        then Right ()
        else Left ("attrValueIs: The actual name \"" <> actual <> "\" does not equal the expected \"" <> expected <> "\"")


-- * Nodes
-------------------------

-- |
-- A consuming sequential monadic parser of tag's nodes.
type Nodes =
  Free AST.Nodes

nodesNode :: Node a -> Nodes a
nodesNode q =
  liftF (AST.NodesNode q id)


-- * Node
-------------------------

type Node =
  Alt AST.Node

nodeTag :: Tag a -> Node a
nodeTag =
  liftAlt . AST.NodeTag

nodeText :: Text a -> Node a
nodeText =
  liftAlt . AST.NodeText
