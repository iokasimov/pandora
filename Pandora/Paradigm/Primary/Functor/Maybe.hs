module Pandora.Paradigm.Primary.Functor.Maybe where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Equal, Greater))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.UT (UT (UT), type (<.:>))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce, resolve))

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

type instance Schematic Monad Maybe = (<.:>) Maybe

instance Interpreted Maybe where
	type Primary Maybe a = Maybe a
	run = identity

instance Monadic Maybe where
	wrap = TM . UT . point

instance Monotonic (Maybe a) a where
	reduce f r (Just x) = f x r
	reduce _ r Nothing = r

instance Monotonic (t a) a => Monotonic (Maybe :. t := a) a where
	reduce f r (Just x) = reduce f r x
	reduce _ r Nothing = r

type Optional = Adaptable Maybe

instance Covariant u => Covariant (Maybe <.:> u) where
	f <$> UT x = UT $ f <$$> x

instance Applicative u => Applicative (Maybe <.:> u) where
	UT f <*> UT x = UT $ apply <$> f <*> x

instance Pointable u => Pointable (Maybe <.:> u) where
	point = UT . point . point

instance (Pointable u, Bindable u) => Bindable (Maybe <.:> u) where
	UT x >>= f = UT $ x >>= resolve (run . f) (point Nothing)

instance Monad u => Monad (Maybe <.:> u) where

nothing :: Optional t => t a
nothing = adapt Nothing
