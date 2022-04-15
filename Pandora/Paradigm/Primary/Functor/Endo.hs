module Pandora.Paradigm.Primary.Functor.Endo where

import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (=#-)))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, (<--))
import Pandora.Pattern.Functor.Invariant (Invariant ((<!<)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Algebraic.Exponential ()
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic ((>-|-<-|-))

newtype Endo a = Endo { endo :: a -> a }

instance Interpreted (->) Endo where
	type Primary Endo a = a -> a
	run ~(Endo x) = x
	unite = Endo

instance Invariant Endo where
	f <!< g = (((g :*: f) >-|-<-|-) =#-)

instance Semigroup (Endo a) where
	Endo f + Endo g = Endo <-- g . f

instance Monoid (Endo a) where
	zero = Endo identity
