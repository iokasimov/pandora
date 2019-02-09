module Pandora.Paradigm.Basis.Cofree (Cofree (..), unwrap, coiterate, section) where

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

data Cofree t a = a :< (t :.: Cofree t) a

instance Covariant t => Covariant (Cofree t) where
	f <$> (x :< xs) = f x :< ((comap . comap) f xs)

instance Exclusive t => Pointable (Cofree t) where
	point x = x :< exclusive

instance Covariant t => Extractable (Cofree t) where
	extract (x :< _) = x

instance Applicative t => Applicative (Cofree t) where
	(f :< fs) <*> (x :< xs) = f x :< ((<*>) <$> fs <*> xs)

instance Traversable t => Traversable (Cofree t) where
	(x :< xs) ->> f = (:<) <$> f x <*> (traverse . traverse) f xs

instance Alternative t => Bindable (Cofree t) where
	(x :< xs) >>= f = case f x of
		y :< ys -> y :< (ys <+> comap (>>= f) xs)

instance Covariant t => Extendable (Cofree t) where
	x =>> f = f x :< comap (extend f) (unwrap x)

instance (Exclusive t, Alternative t) => Monad (Cofree t) where

instance Covariant t => Comonad (Cofree t) where

instance (Setoid a, forall b . Setoid b => Setoid (t b)) => Setoid (Cofree t a) where
	(x :< xs) == (y :< ys) = x == y && xs == ys

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b)) => Semigroup (Cofree t a) where
	(x :< xs) <> (y :< ys) = (x <> y) :< (xs <> ys)

instance (Monoid a, forall b . Semigroup b => Monoid (t b)) => Monoid (Cofree t a) where
	unit = unit :< unit

unwrap :: Cofree t a -> (t :.: Cofree t) a
unwrap (_ :< xs) = xs

coiterate :: Covariant t => (a -> t a) -> a -> Cofree t a
coiterate coalgebra x = x :< (coiterate coalgebra <$> coalgebra x)

section :: Comonad t => t ~> Cofree t
section as = extract as :< extend section as
