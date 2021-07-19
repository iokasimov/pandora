module Pandora.Paradigm.Primary.Functor.Maybe where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative_ (multiply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)), Traversable_ ((-<<--)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)), Bindable_ ((-=<<-)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Equal, Greater))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.UT (UT (UT), type (<.:>))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Functor.Conclusion (Conclusion (Failure, Success))

data Maybe a = Nothing | Just a

instance Covariant Maybe where
	f <$> Just x = Just $ f x
	_ <$> Nothing = Nothing

instance Covariant_ Maybe (->) (->) where
	f -<$>- Just x = Just $ f x
	_ -<$>- Nothing = Nothing

instance Pointable Maybe (->) where
	point = Just

instance Avoidable Maybe where
	empty = Nothing

instance Applicative_ Maybe (:*:) (->) (->) where
	multiply f (Just x :*: Just y) = Just . f $ x :*: y
	multiply _ (Nothing :*: _) = Nothing
	multiply _ (_ :*: Nothing) = Nothing

instance Applicative_ Maybe Conclusion (->) (->) where
	multiply f (Failure (Just x)) = Just . f $ Failure x
	multiply f (Success (Just y)) = Just . f $ Success y
	multiply _ (Failure Nothing) = Nothing
	multiply _ (Success Nothing) = Nothing

instance Alternative Maybe where
	Nothing <+> y = y
	Just x <+> _ = Just x

instance Traversable Maybe where
	Nothing ->> _ = point Nothing
	Just x ->> f = Just <$> f x

instance Traversable_ Maybe (->) (->) where
	_ -<<-- Nothing = point Nothing
	f -<<-- Just x = Just -<$>- f x

instance Bindable Maybe where
	Just x >>= f = f x
	Nothing >>= _ = Nothing

instance Bindable_ Maybe (->) where
	f -=<<- Just x = f x
	_ -=<<- Nothing = Nothing

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
	unite = identity

instance Monadic Maybe where
	wrap = TM . UT . point

instance Monotonic a (Maybe a) where
	reduce f r (Just x) = f x r
	reduce _ r Nothing = r

instance Monotonic a (t a) => Monotonic a (Maybe :. t := a) where
	reduce f r (Just x) = reduce f r x
	reduce _ r Nothing = r

type Optional = Adaptable Maybe

nothing :: Optional t => t a
nothing = adapt Nothing
