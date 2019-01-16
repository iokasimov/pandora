module Paradigm.Basis.Cofree (Cofree (..), unwrap) where

import Core.Morphism ((.))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Traversable (Traversable ((->>), traverse))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Extendable (Extendable ((=>>), extend))
import Pattern.Functor.Monad (Monad)
import Pattern.Functor.Comonad (Comonad)

data Cofree t a = a :< (t (Cofree t a))

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

unwrap :: Cofree t a -> t (Cofree t a)
unwrap (_ :< xs) = xs
