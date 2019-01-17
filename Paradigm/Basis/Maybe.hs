module Paradigm.Basis.Maybe (Maybe (..), maybe) where

import Core.Morphism ((.), ($))
import Core.Variant (Variant (Co))
import Paradigm.Basis.Identity (Identity (Identity))
import Paradigm.Basis.Functor.Transformer (T (T, t), type (:!:))
import Pattern.Functor.Covariant (Covariant ((<$>)))
import Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Traversable (Traversable ((->>)))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Monad (Monad)
import Pattern.Functor.Liftable (Liftable (lift))

data Maybe a = Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe _ f (Just y) = f y

instance Covariant Maybe where
	f <$> Just x = Just $ f x
	f <$> Nothing = Nothing

instance Pointable Maybe where
	point = Just

instance Exclusive Maybe where
	exclusive = Nothing

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

instance (Pointable t, Bindable t) => Bindable (Maybe :!: t) where
	T x >>= f = T $ x >>= maybe (point Nothing) (t . f)

instance (Pointable t, Bindable t) => Monad (Maybe :!: t) where
