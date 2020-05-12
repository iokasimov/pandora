module Pandora.Paradigm.Primary.Functor.Conclusion (Conclusion (..), Failable, conclusion, fail, failure) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Monadic (Monadic (lay, wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))

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

instance Bivariant Conclusion where
	f <-> g = conclusion (Failure . f) (Success . g)

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

fail :: (e -> r) -> Conclusion e ~> Conclusion r
fail f (Failure x) = Failure $ f x
fail _ (Success y) = Success y

instance Interpreted (Conclusion e) where
	type Primary (Conclusion e) a = Conclusion e a
	run x = x

type instance Schematic Monad (Conclusion e) u = UT Covariant Covariant (Conclusion e) u

instance Monadic (Conclusion e) where
	lay x = TM . UT $ Success <$> x
	wrap x = TM . UT . point $ x

type Failable e = Adaptable (Conclusion e)

instance Covariant u => Covariant (UT Covariant Covariant (Conclusion e) u) where
	f <$> UT x = UT $ f <$$> x

instance Applicative u => Applicative (UT Covariant Covariant (Conclusion e) u) where
	UT f <*> UT x = UT $ apply <$> f <*> x

instance Pointable u => Pointable (UT Covariant Covariant (Conclusion e) u) where
	point = UT . point . point

instance (Pointable u, Bindable u) => Bindable (UT Covariant Covariant (Conclusion e) u) where
	UT x >>= f = UT $ x >>= conclusion (point . Failure) (run . f)

instance Monad u => Monad (UT Covariant Covariant (Conclusion e) u) where

failure :: Failable e t => e -> t a
failure = adapt . Failure
