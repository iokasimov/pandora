module Pandora.Pattern.Morphism.Straight where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Core.Appliable (Appliable ((!)))

newtype Straight (v :: * -> * -> *) a e = Straight (v a e)

instance Semigroupoid m => Semigroupoid (Straight m) where
  Straight g . Straight f = Straight (g . f)

instance Category m => Category (Straight m) where
	identity = Straight identity

instance Covariant m m t => Covariant (Straight m) m t where
	(<$>) (Straight f) = (<$>) f

instance Appliable (Straight m) c b m c b where
	(!) (Straight m) = m
