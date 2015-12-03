-- |
-- An API for the implementation of interpreters.
module XML.Query.AST
where

import XML.Query.Prelude hiding (Text)
import qualified XML.Query.Prelude as Prelude


data Text a =
  Text (Prelude.Text -> Either Prelude.Text a)

deriving instance Functor Text


data Tag a =
  TagNameText (Alt Text a) |
  TagAttr (Alt Attr a) |
  TagNodes (Alt Nodes a)

deriving instance Functor Tag


data Attr a =
  AttrNameText (Alt Text a) |
  AttrValueText (Alt Text a)

deriving instance Functor Attr


data Nodes a =
  NodesNode (Alt Node a)

deriving instance Functor Nodes


data Node a =
  NodeTag (Alt Tag a) |
  NodeText (Alt Text a)

deriving instance Functor Node



