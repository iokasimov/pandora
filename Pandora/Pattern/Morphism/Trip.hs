module Pandora.Pattern.Morphism.Trip where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))

newtype Trip (v :: * -> * -> * -> *) a b c = Trip (v c b a)
