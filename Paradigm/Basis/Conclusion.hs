module Paradigm.Basis.Conclusion (Conclusion (..), conclusion) where

import Core.Morphism ((.), ($), (!))
import Paradigm.Basis.Functor.Transformer (T (T, t), type (:!:))
import Pattern.Functor.Covariant (Covariant ((<$>)))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Traversable (Traversable ((->>)))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Monad (Monad)

data Conclusion e a = Failure e | Success a

conclusion :: (e -> b) -> (a -> b) -> Conclusion e a -> b
conclusion f _ (Failure x) = f x
conclusion _ s (Success x) = s x

instance Covariant (Conclusion e) where
	f <$> Success x = Success $ f x
	_ <$> Failure y = Failure y

instance Pointable (Conclusion e) where
	point = Success

instance Applicative (Conclusion e) where
	Success f <*> x = f <$> x
	Failure y <*> x = Failure y

instance Alternative (Conclusion e) where
	Failure y <+> x = x
	Success x <+> y = Success x

instance Traversable (Conclusion e) where
	Failure y ->> _ = point $ Failure y
	Success x ->> f = Success <$> f x

instance Bindable (Conclusion e) where
	Success x >>= f = f x
	Failure y >>= _ = Failure y

instance Monad (Conclusion e) where

instance (Pointable t, Bindable t) => Bindable (Conclusion e :!: t) where
	T x >>= f = T $ x >>= conclusion (point . Failure) (t . f)

instance (Pointable t, Bindable t) => Monad (Conclusion :!: t) where
