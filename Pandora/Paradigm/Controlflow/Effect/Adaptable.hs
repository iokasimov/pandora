{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Controlflow.Effect.Adaptable where

import Pandora.Core.Functor (type (>), type (<))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Transformer (Liftable (lift))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (Pointable, extract, point)
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly)
import Pandora.Paradigm.Controlflow.Effect.Transformer (Monadic, wrap, (:>))

class Adaptable u m t where
	{-# MINIMAL adapt #-}
	adapt :: m < t a < u a

instance Category m => Adaptable t m t where
	adapt = identity @m

instance {-# OVERLAPS #-} Monoidal (-->) (-->) (:*:) (:*:) u => Adaptable u (->) Exactly where
	adapt = point . extract

class Effectful m v t u where
	effect :: m (v a) (t :> u > a)

instance (Pointable u, Monadic m t) => Effectful m t t u where
	effect = wrap

instance (Covariant m m u, Liftable m ((:>) t)) => Effectful m u t u where
	effect = lift

instance {-# OVERLAPS #-} (Semigroupoid m, Effectful m u t u, Adaptable u m v) => Effectful m v t u where
	effect = effect @m @u @t @u . adapt @u @m @v

instance Effectful m v t u => Adaptable (t :> u) m v where
	adapt = effect @m @v @t @u
