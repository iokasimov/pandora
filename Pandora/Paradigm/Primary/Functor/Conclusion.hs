module Pandora.Paradigm.Primary.Functor.Conclusion where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.UT (UT (UT), type (<.:>))

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

type instance Schematic Monad (Conclusion e) = (<.:>) (Conclusion e)

instance Monadic (Conclusion e) where
	wrap x = TM . UT . point $ x

type Failable e = Adaptable (Conclusion e)

instance Covariant u => Covariant (Conclusion e <.:> u) where
	f <$> UT x = UT $ f <$$> x

instance Applicative u => Applicative (Conclusion e <.:> u) where
	UT f <*> UT x = UT $ (<*>) <$> f <*> x

instance Pointable u => Pointable (Conclusion e <.:> u) where
	point = UT . point . point

instance (Pointable u, Bindable u) => Bindable (Conclusion e <.:> u) where
	UT x >>= f = UT $ x >>= conclusion (point . Failure) (run . f)

instance Monad u => Monad (Conclusion e <.:> u) where

failure :: Failable e t => e -> t a
failure = adapt . Failure

class Catchable e t where
	catch :: t a -> (e -> t a) -> t a

instance Catchable e (Conclusion e) where
	catch (Failure e) handle = handle e
	catch (Success x) _ = Success x

instance Monad u => Catchable e (Conclusion e <.:> u) where
	catch (UT x) handle = UT $ x >>= conclusion (run . handle) (point . Success)
