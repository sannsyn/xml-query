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
  Free AST.Text

-- |
-- Lifts an arbitrary textual parser function to a text-value parser.
-- 
-- Provides a doorway for composition with such libraries as \"parsec\" or \"attoparsec\".
makeFreeCon_ 'AST.Text
text :: (Prelude.Text -> Either Prelude.Text a) -> Text a

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
  Free AST.Tag

makeFreeCon_ 'AST.TagNameText
tagNameText :: Text a -> Tag a

makeFreeCon_ 'AST.TagAttr
tagAttr :: Attr a -> Tag a

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
  Free AST.Attr


-- * Nodes
-------------------------
