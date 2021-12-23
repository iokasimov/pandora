module Pandora.Paradigm.Primary.Functor.Maybe where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, (!))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
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
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point)

data Maybe a = Nothing | Just a

instance Covariant (->) (->) Maybe where
	f <-|- Just x = Just ! f x
	_ <-|- Nothing = Nothing

instance Semimonoidal (-->) (:*:) (:*:) Maybe where
	mult = Straight ! \case
		Just x :*: Just y -> Just ! x :*: y
		Nothing :*: _ -> Nothing
		_ :*: Nothing -> Nothing

instance Semimonoidal (-->) (:*:) (:+:) Maybe where
	mult = Straight ! \case
		Just x :*: _ -> Just ! Option x
		Nothing :*: Just y -> Just ! Adoption y
		Nothing :*: Nothing -> Nothing

instance Monoidal (-->) (-->) (:*:) (:*:) Maybe where
	unit _ = Straight ! Just . (! One) . run

instance Monoidal (-->) (-->) (:*:) (:+:) Maybe where
	unit _ = Straight ! \_ -> Nothing

-- TODO: Check laws
instance Semimonoidal (<--) (:*:) (:*:) Maybe where
	mult = Flip ! \case
		Just (x :*: y) -> Just x :*: Just y
		Nothing -> Nothing :*: Nothing

instance Traversable (->) (->) Maybe where
	_ <<- Nothing = point Nothing
	f <<- Just x = Just <-|- f x

instance Bindable (->) Maybe where
	f =<< Just x = f x
	_ =<< Nothing = Nothing

--instance Monad Maybe where

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
	Just x + Just y = Just ! x + y
	Nothing + x = x
	x + Nothing = x

instance Semigroup a => Monoid (Maybe a) where
	zero = Nothing

instance Infimum a => Infimum (Maybe a) where
	Just x /\ Just y = Just ! x /\ y
	_ /\ Nothing = Nothing
	Nothing /\ _ = Nothing

instance Supremum a => Supremum (Maybe a) where
	Just x \/ Just y = Just ! x \/ y
	x \/ Nothing = x
	Nothing \/ x = x

instance Lattice a => Lattice (Maybe a) where

type instance Schematic Monad Maybe = (<.:>) Maybe

instance Interpreted (->) Maybe where
	type Primary Maybe a = Maybe a
	run = identity
	unite = identity

instance Monadic (->) Maybe where
	wrap = TM . UT . point

instance Monotonic a (Maybe a) where
	reduce f r (Just x) = f x r
	reduce _ r Nothing = r

instance Monotonic a (t a) => Monotonic a (Maybe :. t := a) where
	reduce f r (Just x) = reduce f r x
	reduce _ r Nothing = r

type Optional t = Adaptable t (->) Maybe

nothing :: Optional t => t a
nothing = adapt Nothing
