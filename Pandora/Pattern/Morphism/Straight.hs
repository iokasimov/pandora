module Pandora.Pattern.Morphism.Straight where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Core.Appliable (Appliable ((!)))

newtype Straight (v :: * -> * -> *) a e = Straight (v a e)

instance Semigroupoid m => Semigroupoid (Straight m) where
  Straight g . Straight f = Straight (g . f)

instance Category m => Category (Straight m) where
	identity = Straight identity

instance Appliable (Straight m) c b m c b where
	(!) (Straight m) = m
