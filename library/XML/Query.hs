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


-- * Element
-------------------------

type Element =
  Alt AST.Element

elementNameText :: Text a -> Element a
elementNameText =
  liftAlt . AST.ElementNameText

-- |
-- Parses one of element's attributes without any regard to order.
elementAttr :: Attr a -> Element a
elementAttr =
  liftAlt . AST.ElementAttr

-- |
-- Parses all of element's nodes.
-- 
-- Can be used multiple times,
-- thus allowing for parallel parsing of element's child-nodes.
-- Naturally this will result in traversing the element's nodes multiple times.
elementNodes :: Nodes a -> Element a
elementNodes =
  liftAlt . AST.ElementNodes

-- ** Derivatives
-------------------------

elementNameIs :: Prelude.Text -> Element ()
elementNameIs expected =
  elementNameText (text textParserFn)
  where
    textParserFn actual =
      if actual == expected
        then Right ()
        else Left ("elementNameIs: The actual name \"" <> actual <> "\" does not equal the expected \"" <> expected <> "\"")


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
-- A sequential backtracking parser of nodes.
type Nodes =
  Alt AST.Nodes

-- |
-- Parses the next node.
nodesImmediateNode :: Node a -> Nodes a
nodesImmediateNode =
  liftAlt . AST.NodesNode

-- |
-- Parses one of the following nodes.
nodesEventualNode :: Node a -> Nodes a
nodesEventualNode node =
  fix $ \loop ->
    nodesImmediateNode node <|> (nodesImmediateNode (pure ()) *> loop)


-- * Node
-------------------------

type Node =
  Alt AST.Node

nodeElement :: Element a -> Node a
nodeElement =
  liftAlt . AST.NodeElement

nodeText :: Text a -> Node a
nodeText =
  liftAlt . AST.NodeText
