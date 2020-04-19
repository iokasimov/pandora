module Pandora.Paradigm.Controlflow.Joint.Schemes.TUT (TUT (..)) where

import Pandora.Pattern.Category (identity)
import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)
import Pandora.Pattern.Functor.Applicative (Applicative)
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype TUT ct ct' cu t t' u a = TUT (t :. u :. t' := a)

instance Interpreted (TUT ct ct' cu t t' u) where
	type Primary (TUT ct ct' cu t t' u) a = t :. u :. t' := a
	run (TUT x) = x

instance (Adjoint t' t, Applicative t, Pointable t, forall u . Traversable u)
	=> Liftable (TUT Covariant Covariant Covariant t t') where
		lift x = TUT $ x ->> (-| identity)

instance (Adjoint t t', Distributive t')
	=> Lowerable (TUT Covariant Covariant Covariant t t') where
		lower (TUT x) = x |- (>>- identity)