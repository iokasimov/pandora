module Pandora.Paradigm.Basis.Conclusion (Conclusion (..), conclusion, fail) where

import Pandora.Core.Functor (Variant (Co))
import Pandora.Core.Morphism ((.))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))
import Pandora.Pattern.Junction.Transformer (Transformer (Schema, lay, wrap))
import Pandora.Pattern.Junction.Schemes.UT (UT (UT))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)), Boolean (False))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), Ordering (Less, Greater))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))

data Conclusion e a = Failure e | Success a

instance Covariant (Conclusion e) where
	f <$> Success x = Success $ f x
	_ <$> Failure y = Failure y

instance Pointable (Conclusion e) where
	point = Success

instance Applicative (Conclusion e) where
	Success f <*> x = f <$> x
	Failure y <*> _ = Failure y

instance Alternative (Conclusion e) where
	Failure _ <+> x = x
	Success x <+> _ = Success x

instance Traversable (Conclusion e) where
	Failure y ->> _ = point $ Failure y
	Success x ->> f = Success <$> f x

instance Bindable (Conclusion e) where
	Success x >>= f = f x
	Failure y >>= _ = Failure y

instance Monad (Conclusion e) where

instance Composition (Conclusion e) where
	type Primary (Conclusion e) a = Conclusion e a
	unwrap x = x

instance Transformer (Conclusion e) where
	type Schema (Conclusion e) u = UT 'Co 'Co (Conclusion e) u
	lay x = UT $ Success <$> x
	wrap x = UT . point $ x

instance Covariant u => Covariant (UT 'Co 'Co (Conclusion e) u) where
	f <$> UT x = UT $ f <$$> x

instance Applicative u => Applicative (UT 'Co 'Co (Conclusion e) u) where
	UT f <*> UT x = UT $ apply <$> f <*> x

instance Pointable u => Pointable (UT 'Co 'Co (Conclusion e) u) where
	point = UT . point . point

instance (Pointable u, Bindable u) => Bindable (UT 'Co 'Co (Conclusion e) u) where
	UT x >>= f = UT $ x >>= conclusion (point . Failure) (unwrap . f)

instance Monad u => Monad (UT 'Co 'Co (Conclusion e) u) where

instance (Setoid e, Setoid a) => Setoid (Conclusion e a) where
	Success x == Success y = x == y
	Failure x == Failure y = x == y
	_ == _ = False

instance (Chain e, Chain a) => Chain (Conclusion e a) where
	Success x <=> Success y = x <=> y
	Failure x <=> Failure y = x <=> y
	Failure _ <=> Success _ = Less
	Success _ <=> Failure _ = Greater

instance (Semigroup e, Semigroup a) => Semigroup (Conclusion e a) where
	Success x + Success y = Success $ x + y
	Failure x + Failure y = Failure $ x + y
	Failure _ + Success y = Success y
	Success x + Failure _ = Success x

conclusion :: (e -> r) -> (a -> r) -> Conclusion e a -> r
conclusion f _ (Failure x) = f x
conclusion _ s (Success x) = s x

fail :: (e -> r) -> Conclusion e a -> Conclusion r a
fail f (Failure x) = Failure $ f x
fail _ (Success y) = Success y
