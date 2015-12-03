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

tagAttr :: Attr a -> Tag a
tagAttr =
  liftAlt . AST.TagAttr

tagNodes :: Nodes a -> Tag a
tagNodes =
  liftAlt . AST.TagNodes

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

attrNameText :: Text a -> Attr a
attrNameText =
  liftAlt . AST.AttrNameText

attrValueText :: Text a -> Attr a
attrValueText =
  liftAlt . AST.AttrValueText

-- ** Derivatives
-------------------------

attrNameIs :: Prelude.Text -> Attr ()
attrNameIs expected =
  attrNameText (text textParserFn)
  where
    textParserFn actual =
      if actual == expected
        then Right ()
        else Left ("attrNameIs: The actual name \"" <> actual <> "\" does not equal the expected \"" <> expected <> "\"")

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

type Nodes =
  Alt AST.Nodes

nodesNode :: Node a -> Nodes a
nodesNode =
  liftAlt . AST.NodesNode


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
