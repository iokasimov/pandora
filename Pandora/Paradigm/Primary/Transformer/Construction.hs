module Pandora.Paradigm.Primary.Transformer.Construction (Construction (..), deconstruct, coiterate, section) where

import Pandora.Core.Functor (type (:.), type (:=), type (|->), type (~>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>), extend))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid ((*))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Schemes.TU (TU (TU))

data Construction t a = Construct a (t :. Construction t := a)

instance Covariant t => Covariant (Construction t) where
	f <$> Construct x xs = Construct (f x) $ f <$$> xs

instance Avoidable t => Pointable (Construction t) where
	point x = Construct x empty

instance Covariant t => Extractable (Construction t) where
	extract (Construct x _) = x

instance Applicative t => Applicative (Construction t) where
	Construct f fs <*> Construct x xs = Construct (f x) $ fs <**> xs

instance Traversable t => Traversable (Construction t) where
	Construct x xs ->> f = Construct <$> f x <*> xs ->>> f

instance Alternative t => Bindable (Construction t) where
	Construct x xs >>= f = case f x of Construct y ys -> Construct y $ ys <+> (>>= f) <$> xs

instance Covariant t => Extendable (Construction t) where
	x =>> f = Construct (f x) $ extend f <$> deconstruct x

instance (Avoidable t, Alternative t) => Monad (Construction t) where

instance Covariant t => Comonad (Construction t) where

instance Lowerable Construction where
	lower (Construct _ xs) = extract <$> xs

instance Hoistable Construction where
	hoist f (Construct x xs) = Construct x . f $ hoist f <$> xs

instance (Setoid a, forall b . Setoid b => Setoid (t b)) => Setoid (Construction t a) where
	Construct x xs == Construct y ys = (x == y) * (xs == ys)

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b)) => Semigroup (Construction t a) where
	Construct x xs + Construct y ys = Construct (x + y) $ xs + ys

instance (Monoid a, forall b . Semigroup b => Monoid (t b)) => Monoid (Construction t a) where
	zero = Construct zero zero

deconstruct :: Construction t a -> (t :. Construction t) a
deconstruct (Construct _ xs) = xs

coiterate :: Covariant t => a |-> t -> a |-> Construction t
coiterate coalgebra x = Construct x $ coiterate coalgebra <$> coalgebra x

section :: Comonad t => t ~> Construction t
section as = Construct (extract as) $ extend section as

instance (Covariant t, Covariant u) => Covariant (TU Covariant Covariant u (Construction t)) where
	f <$> TU g = TU $ f <$$> g

instance (Avoidable t, Pointable u) => Pointable (TU Covariant Covariant u (Construction t)) where
	point x = TU . point . Construct x $ empty

instance (Applicative t, Applicative u) => Applicative (TU Covariant Covariant u (Construction t)) where
	TU f <*> TU x = TU $ f <**> x

instance (Covariant t, Alternative u) => Alternative (TU Covariant Covariant u (Construction t)) where
	TU x <+> TU y = TU $ x <+> y

instance (Covariant t, Avoidable u) => Avoidable (TU Covariant Covariant u (Construction t)) where
	empty = TU empty

instance (Traversable t, Traversable u) => Traversable (TU Covariant Covariant u (Construction t)) where
	TU g ->> f = TU <$> g ->>> f
