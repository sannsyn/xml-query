-- |
-- An API for the implementation of interpreters.
module XML.Query.AST
where

import XML.Query.Prelude hiding (Text)
import qualified XML.Query.Prelude as Prelude


data Text x =
  forall a. Text (Prelude.Text -> Either Prelude.Text a) (a -> x)

deriving instance Functor Text
