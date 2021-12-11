{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Effect.Adaptable where

import Pandora.Core.Functor (type (#))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Transformer (Liftable (lift), Lowerable (lower))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (Extractable, Pointable, extract, point)
import Pandora.Paradigm.Primary.Functor.Identity (Identity)
import Pandora.Paradigm.Controlflow.Effect.Transformer (Monadic, Comonadic, wrap, bring, (:>), (:<))

class Adaptable m t u where
	{-# MINIMAL adapt #-}
	adapt :: m (t a) (u a)

instance Category m => Adaptable m t t where
	adapt = identity @m

instance {-# OVERLAPPING #-} Monoidal (-->) (-->) (:*:) (:*:) u => Adaptable (->) Identity u where
	adapt = point . extract
