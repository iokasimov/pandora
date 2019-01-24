module Pandora.Paradigm.Basis.Conclusion (Conclusion (..), conclusion) where

import Pandora.Core.Morphism ((.), ($), (!))
import Pandora.Paradigm.Basis.Junction.Transformer (T (T, t), type (:!:))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)

data Conclusion e a = Failure e | Success a

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

instance Monad t => Monad (Conclusion e :!: t) where

conclusion :: (e -> r) -> (a -> r) -> Conclusion e a -> r
conclusion f _ (Failure x) = f x
conclusion _ s (Success x) = s x
