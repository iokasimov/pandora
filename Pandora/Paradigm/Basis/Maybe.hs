module Pandora.Paradigm.Basis.Maybe (Maybe (..), maybe) where

import Pandora.Core.Functor (Variant (Co))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Identity (Identity (Identity))
import Pandora.Paradigm.Basis.Junction.Transformer (T (T, t), type (:!:))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Liftable (Liftable (lift))

data Maybe a = Nothing | Just a

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

instance Monad t => Monad (Maybe :!: t) where

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe _ f (Just y) = f y
