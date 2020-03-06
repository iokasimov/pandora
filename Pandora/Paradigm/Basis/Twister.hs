module Pandora.Paradigm.Basis.Twister (Twister (..), untwist, coiterate, section) where

import Pandora.Core.Functor (type (:.), type (:=), type (|->))
import Pandora.Core.Transformation (type (~>))
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
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)), (&&))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))

data Twister t a = Twister a (t :. Twister t := a)

instance Covariant t => Covariant (Twister t) where
	f <$> Twister x xs = Twister (f x) (f <$$> xs)

instance Avoidable t => Pointable (Twister t) where
	point x = Twister x empty

instance Covariant t => Extractable (Twister t) where
	extract (Twister x _) = x

instance Applicative t => Applicative (Twister t) where
	Twister f fs <*> Twister x xs = Twister (f x) $ fs <**> xs

instance Traversable t => Traversable (Twister t) where
	Twister x xs ->> f = Twister <$> f x <*> xs ->>> f

instance Alternative t => Bindable (Twister t) where
	Twister x xs >>= f = case f x of Twister y ys -> Twister y $ ys <+> (>>= f) <$> xs

instance Covariant t => Extendable (Twister t) where
	x =>> f = Twister (f x) $ extend f <$> untwist x

instance (Avoidable t, Alternative t) => Monad (Twister t) where

instance Covariant t => Comonad (Twister t) where

instance (Setoid a, forall b . Setoid b => Setoid (t b)) => Setoid (Twister t a) where
	Twister x xs == Twister y ys = x == y && xs == ys

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b)) => Semigroup (Twister t a) where
	Twister x xs + Twister y ys = Twister (x + y) $ xs + ys

instance (Monoid a, forall b . Semigroup b => Monoid (t b)) => Monoid (Twister t a) where
	zero = Twister zero zero

untwist :: Twister t a -> (t :. Twister t) a
untwist (Twister _ xs) = xs

coiterate :: Covariant t => a |-> t -> a -> Twister t a
coiterate coalgebra x = Twister x $ coiterate coalgebra <$> coalgebra x

section :: Comonad t => t ~> Twister t
section as = Twister (extract as) $ extend section as
