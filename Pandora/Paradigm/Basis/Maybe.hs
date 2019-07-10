module Pandora.Paradigm.Basis.Maybe (Maybe (..), maybe) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Junction.Composition (composition)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (idle))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Object.Setoid (Setoid ((==)), Boolean (True, False))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), Ordering (Less, Equal, Greater))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)

data Maybe a = Nothing | Just a

instance Covariant Maybe where
	f <$> Just x = Just $ f x
	_ <$> Nothing = Nothing

instance Pointable Maybe where
	point = Just

instance Avoidable Maybe where
	idle = Nothing

instance Applicative Maybe where
	Just f <*> x = f <$> x
	Nothing <*> _ = Nothing

instance Alternative Maybe where
	Nothing <+> y = y
	Just x <+> _ = Just x

instance Traversable Maybe where
	Nothing ->> _ = point Nothing
	Just x ->> f = Just <$> f x

instance Bindable Maybe where
	Just x >>= f = f x
	Nothing >>= _ = Nothing

instance Monad Maybe where

instance Setoid a => Setoid (Maybe a) where
	Just x == Just y = x == y
	Nothing == Nothing = True
	_ == _ = False

instance Chain a => Chain (Maybe a) where
	Just x <=> Just y = x <=> y
	Nothing <=> Nothing = Equal
	Nothing <=> Just _ = Less
	Just _ <=> Nothing = Greater

instance Semigroup a => Semigroup (Maybe a) where
	Just x + Just y = Just $ x + y
	Nothing + x = x
	x + Nothing = x

instance Semigroup a => Monoid (Maybe a) where
	zero = Nothing

instance Infimum a => Infimum (Maybe a) where
	Just x /\ Just y = Just $ x /\ y
	_ /\ Nothing = Nothing
	Nothing /\ _ = Nothing

instance Supremum a => Supremum (Maybe a) where
	Just x \/ Just y = Just $ x \/ y
	x \/ Nothing = x
	Nothing \/ x = x

instance Lattice a => Lattice (Maybe a) where

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe _ f (Just y) = f y
