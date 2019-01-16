module Paradigm.Maybe (Maybe (..)) where

import Core.Morphism (($))
import Pattern.Functor.Covariant (Covariant ((<$>)))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Traversable (Traversable ((->>)))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Monad (Monad)

data Maybe a = Nothing | Just a

instance Covariant Maybe where
	f <$> Just x = Just $ f x
	f <$> Nothing = Nothing

instance Pointable Maybe where
	point = Just

instance Applicative Maybe where
	Just f <*> x = f <$> x
	Nothing <*> x = Nothing

instance Alternative Maybe where
	Nothing <+> y = y
	Just x <+> y = Just x

instance Traversable Maybe where
	Nothing ->> _ = point Nothing
	Just x ->> f = Just <$> f x

instance Bindable Maybe where
	Just x >>= f = f x
	Nothing >>= _ = Nothing

instance Monad Maybe where
