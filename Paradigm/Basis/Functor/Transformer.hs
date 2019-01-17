module Paradigm.Basis.Functor.Transformer (UT (..)) where

import Core.Transformer ((:!:))
import Core.Morphism ((.), ($))
import Core.Variant (Variant (Co, Contra))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Contravariant (Contravariant ((>$<), contramap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pattern.Functor.Adjoint (Adjoint (phi, psi))


newtype UT ct cu t u a = UT { ut :: (t :!: u) a }

instance (Covariant t, Covariant u) => Covariant (UT Co Co t u) where
	f <$> UT x = UT $ (comap . comap) f x

instance (Contravariant t, Covariant u) => Contravariant (UT Co Contra t u) where
	f >$< UT x = UT $ contramap f <$> x

instance (Covariant t, Contravariant u) => Contravariant (UT Contra Co t u) where
	f >$< UT x = UT $ contramap (comap f) x

instance (Contravariant t, Contravariant u) => Covariant (UT Contra Contra t u) where
	f <$> UT x = UT $ contramap (contramap f) x
