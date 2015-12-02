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
  TagNameText (Text a) |
  TagAttr (Attr a) |
  TagNodes (Nodes a)

deriving instance Functor Tag


data Attr a =
  AttrNameText (Text a) |
  AttrValueText (Text a)

deriving instance Functor Attr


data Nodes a =
  NodesNode (Node a)

deriving instance Functor Nodes


data Node a =
  NodeTag (Tag a) |
  NodeText (Text a) |
  NodeSpace (Text a)

deriving instance Functor Node



