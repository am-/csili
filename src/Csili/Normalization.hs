
module Csili.Normalization
( normalize
) where

import qualified Csili.Normalization.Capacity as Capacity
import qualified Csili.Normalization.Marking as Marking
import Csili.Semantics

normalize :: Semantics -> Semantics
normalize = Marking.normalize . Capacity.normalize
