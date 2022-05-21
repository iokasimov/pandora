module Pandora.Pattern.Morphism.Kleisli where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))

newtype Kleisli t (v :: * -> * -> *) a e = Kleisli (v a (t e))
