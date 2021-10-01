module Pandora.Pattern.Morphism.Flip where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Core.Appliable (Appliable ((!)))

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)

instance Semigroupoid m => Semigroupoid (Flip m) where
  Flip g . Flip f = Flip (f . g)

instance Category m => Category (Flip m) where
	identity = Flip identity

instance (Category m, Covariant m m t) => Contravariant (Flip m) m t where
	(>$<) (Flip f) = (<$>) f

instance (Category m, Covariant m m t) => Contravariant m (Flip m) t where
	(>$<) f = Flip ((<$>) f)

instance (Category m, Covariant m m t) => Covariant  (Flip m) (Flip m) t where
	(<$>) (Flip f) = Flip ((<$>) f)

instance Appliable (Flip m) b c m c b where
	(!) (Flip m) = m
