-- |
-- An API for the implementation of interpreters.
module XML.Query.AST
where

import XML.Query.Prelude hiding (Text)
import qualified XML.Query.Prelude as Prelude


data Text x =
  forall a. Text (Prelude.Text -> Either Prelude.Text a) (a -> x)

deriving instance Functor Text


data Tag x =
  forall a. TagNameText (Free Text a) (a -> x) |
  forall a. TagAttr (Free Attr a) (a -> x) |
  forall a. TagNodes (Free Nodes a) (a -> x)

deriving instance Functor Tag


data Attr x =
  forall a. AttrNameText (Free Text a) (a -> x) |
  forall a. AttrValueText (Free Text a) (a -> x)

deriving instance Functor Attr


data Nodes x =
  forall a. NodesNode (Free Node a) (a -> x)

deriving instance Functor Nodes


data Node x =
  forall a. NodeTag (Free Tag a) (a -> x) |
  forall a. NodeText (Free Text a) (a -> x) |
  forall a. NodeSpace (Free Text a) (a -> x)

deriving instance Functor Node



