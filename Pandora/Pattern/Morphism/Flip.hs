module Pandora.Pattern.Morphism.Flip where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Core.Appliable (Appliable ((!)))

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)

instance Semigroupoid m => Semigroupoid (Flip m) where
  Flip g . Flip f = Flip (f . g)

instance Category m => Category (Flip m) where
	identity = Flip identity

instance Appliable (Flip m) b c m c b where
	(!) (Flip m) = m
