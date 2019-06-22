module Pandora.Paradigm.Basis.Twister (Twister (..), unwrap, coiterate, section) where

import Pandora.Core.Functor (type (:.:), type (~>))
import Pandora.Core.Morphism ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), traverse))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>), extend))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Object.Setoid (Setoid ((==)), (&&))
import Pandora.Pattern.Object.Semigroup (Semigroup ((<>)))
import Pandora.Pattern.Object.Monoid (Monoid (unit))

data Twister t a = a :< (t :.: Twister t) a

instance Covariant t => Covariant (Twister t) where
	f <$> (x :< xs) = f x :< ((comap . comap) f xs)

instance Exclusive t => Pointable (Twister t) where
	point x = x :< exclusive

instance Covariant t => Extractable (Twister t) where
	extract (x :< _) = x

instance Applicative t => Applicative (Twister t) where
	(f :< fs) <*> (x :< xs) = f x :< ((<*>) <$> fs <*> xs)

instance Traversable t => Traversable (Twister t) where
	(x :< xs) ->> f = (:<) <$> f x <*> (traverse . traverse) f xs

instance Alternative t => Bindable (Twister t) where
	(x :< xs) >>= f = case f x of
		y :< ys -> y :< (ys <+> comap (>>= f) xs)

instance Covariant t => Extendable (Twister t) where
	x =>> f = f x :< comap (extend f) (unwrap x)

instance (Exclusive t, Alternative t) => Monad (Twister t) where

instance Covariant t => Comonad (Twister t) where

instance (Setoid a, forall b . Setoid b => Setoid (t b)) => Setoid (Twister t a) where
	(x :< xs) == (y :< ys) = x == y && xs == ys

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b)) => Semigroup (Twister t a) where
	(x :< xs) <> (y :< ys) = (x <> y) :< (xs <> ys)

instance (Monoid a, forall b . Semigroup b => Monoid (t b)) => Monoid (Twister t a) where
	unit = unit :< unit

unwrap :: Twister t a -> (t :.: Twister t) a
unwrap (_ :< xs) = xs

coiterate :: Covariant t => (a -> t a) -> a -> Twister t a
coiterate coalgebra x = x :< (coiterate coalgebra <$> coalgebra x)

section :: Comonad t => t ~> Twister t
section as = extract as :< extend section as
