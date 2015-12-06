module XMLQuery.Prelude
( 
  module Exports,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (fail, Alt)

-- text
-------------------------
import Data.Text as Exports (Text)

-- free
-------------------------
import Control.Alternative.Free as Exports
import Control.Monad.Free as Exports hiding (Pure)
import Control.Monad.Free.TH as Exports
