{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Construction where

import Pandora.Core.Functor (type (:.), type (:=), type (:=>), type (~>))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=), ($>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>), extend))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\), hoist))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid ((*))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Functor.Function ()
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce, resolve))

infixr 7 .-+

data Construction t a = Construct a (t :. Construction t := a)

instance Covariant t => Covariant (Construction t) where
	f <$> ~(Construct x xs) = Construct # f x # f <$$> xs

instance Avoidable t => Pointable (Construction t) where
	point x = Construct x empty

instance Covariant t => Extractable (Construction t) where
	extract ~(Construct x _) = x

instance Applicative t => Applicative (Construction t) where
	~(Construct f fs) <*> ~(Construct x xs) = Construct # f x # fs <**> xs

instance Traversable t => Traversable (Construction t) where
	~(Construct x xs) ->> f = Construct <$> f x <*> xs ->>> f

instance Alternative t => Bindable (Construction t) where
	~(Construct x xs) >>= f = Construct # extract (f x) # deconstruct (f x) <+> xs $>>= f

instance Covariant t => Extendable (Construction t) where
	x =>> f = Construct # f x # extend f <$> deconstruct x

instance (Avoidable t, Alternative t) => Monad (Construction t) where

instance Covariant t => Comonad (Construction t) where

instance Lowerable Construction where
	lower x = extract <$> deconstruct x

instance Hoistable Construction where
	f /|\ x = Construct # extract x $ f # hoist f <$> deconstruct x

instance (Setoid a, forall b . Setoid b => Setoid (t b), Covariant t) => Setoid (Construction t a) where
	x == y = (extract x == extract y) * (deconstruct x == deconstruct y)

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b), Covariant t) => Semigroup (Construction t a) where
	x + y = Construct # extract x + extract y # deconstruct x + deconstruct y

instance (Monoid a, forall b . Semigroup b => Monoid (t b), Covariant t) => Monoid (Construction t a) where
	zero = Construct zero zero

instance Monotonic a (t :. Construction t := a) => Monotonic a (Construction t a) where
	reduce f r ~(Construct x xs) = f x $ reduce f r xs

deconstruct :: Construction t a -> t :. Construction t := a
deconstruct ~(Construct _ xs) = xs

-- Generate a construction from seed using effectful computation
(.-+) :: Covariant t => a :=> t -> a :=> Construction t
f .-+ x = Construct x $ (f .-+) <$> f x

section :: Comonad t => t ~> Construction t
section xs = Construct # extract xs $ xs =>> section
