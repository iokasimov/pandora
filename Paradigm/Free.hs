module Paradigm.Free (Free (..)) where

import Core.Morphism ((.), ($))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Traversable (Traversable ((->>), traverse))
import Pattern.Functor.Bindable (Bindable ((>>=)))

data Free t a = Point a | Free (t (Free t a))

instance Covariant t => Covariant (Free t) where
	f <$> Point x = Point $ f x
	f <$> Free xs = Free $ (comap . comap) f xs

instance Covariant t => Pointable (Free t) where
	point = Point

instance Alternative t => Alternative (Free t) where
	Point x <+> _ = Point x
	_ <+> Point y = Point y
	Free xs <+> Free ys = Free $ xs <+> ys

instance Exclusive t => Exclusive (Free t) where
	exclusive = Free exclusive

instance Covariant t => Applicative (Free t) where
	Point f <*> Point y = Point $ f y
	Point f <*> Free y = Free $ comap f <$> y
	Free f <*> y = Free $ (<*> y) <$> f

instance Covariant t => Bindable (Free t) where
	Point x >>= f = f x
	Free xs >>= f = Free $ (>>= f) <$> xs

instance Traversable t => Traversable (Free t) where
	Point x ->> f = Point <$> f x
	Free xs ->> f = Free <$> (traverse . traverse) f xs