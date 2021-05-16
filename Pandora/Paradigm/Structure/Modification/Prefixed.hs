{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Modification.Prefixed where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Into), premorph)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)

newtype Prefixed t k a = Prefixed (t :. Product k := a)

instance Interpreted (Prefixed t k) where
	type Primary (Prefixed t k) a = t :. Product k := a
	run ~(Prefixed x) = x
	unite = Prefixed

instance Covariant t => Covariant (Prefixed t k) where
	f <$> Prefixed x = Prefixed $ f <$$> x

instance Traversable t => Traversable (Prefixed t k) where
	Prefixed x ->> f = Prefixed <$> x ->>> f

instance (Monoid k, Pointable t) => Pointable (Prefixed t k) where
	point = Prefixed . point . (:*:) zero

instance Alternative t => Alternative (Prefixed t k) where
	x <+> y = Prefixed $ run x <+> run y

instance Avoidable t => Avoidable (Prefixed t k) where
	empty = Prefixed empty

instance Covariant t => Morphable (Into t) (Prefixed t k) where
	type Morphing (Into t) (Prefixed t k) = t
	morphing (run . premorph -> prefixed) = extract <$> prefixed

type instance Nonempty (Prefixed t k) = Prefixed (Nonempty t) k
