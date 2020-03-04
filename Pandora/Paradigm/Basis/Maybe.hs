module Pandora.Paradigm.Basis.Maybe (Maybe (..), Optional, maybe, nothing) where

import Pandora.Core.Functor (Variant (Co))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Paradigm.Controlflow.Joint.Monadic (Monadic (lay, wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Divariant (($))
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
	empty = Nothing

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

type instance Schematic Monad Maybe u = UT 'Co 'Co Maybe u

instance Interpreted Maybe where
	type Primary Maybe a = Maybe a
	unwrap x = x

instance Monadic Maybe where
	lay x = TM . UT $ Just <$> x
	wrap x = TM . UT . point $ x

type Optional = Adaptable Maybe

instance Covariant u => Covariant (UT 'Co 'Co Maybe u) where
	f <$> UT x = UT $ f <$$> x

instance Applicative u => Applicative (UT 'Co 'Co Maybe u) where
	UT f <*> UT x = UT $ apply <$> f <*> x

instance Pointable u => Pointable (UT 'Co 'Co Maybe u) where
	point = UT . point . point

instance (Pointable u, Bindable u) => Bindable (UT 'Co 'Co Maybe u) where
	UT x >>= f = UT $ x >>= maybe (point Nothing) (unwrap . f)

instance Monad u => Monad (UT 'Co 'Co Maybe u) where

nothing :: (Covariant t, Optional t) => t a
nothing = adapt Nothing
