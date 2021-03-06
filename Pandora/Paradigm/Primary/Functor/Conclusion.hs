module Pandora.Paradigm.Primary.Functor.Conclusion where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category (identity, (.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)), Applicative_ (multiply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)), Traversable_ ((-<<--)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)), Bindable_ (join_))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)), Bivariant_ ((-<->-)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.UT (UT (UT), type (<.:>))

data Conclusion e a = Failure e | Success a

instance Covariant (Conclusion e) where
	f <$> Success x = Success $ f x
	_ <$> Failure y = Failure y

instance Covariant_ (Conclusion e) (->) (->) where
	f -<$>- Success x = Success $ f x
	_ -<$>- Failure y = Failure y

instance Covariant_ (Flip Conclusion e) (->) (->) where
	_ -<$>- Flip (Success x) = Flip $ Success x
	f -<$>- Flip (Failure y) = Flip . Failure $ f y

instance Pointable (Conclusion e) (->) where
	point = Success

instance Applicative (Conclusion e) where
	Success f <*> x = f <$> x
	Failure y <*> _ = Failure y

instance Applicative_ (Conclusion e) (:*:) (->) (->) where
	multiply f (Success x :*: Success y) = Success . f $ x :*: y
	multiply _ (Failure x :*: _) = Failure x
	multiply _ (_ :*: Failure x) = Failure x

instance Applicative_ (Conclusion e) Conclusion (->) (->) where
	multiply f (Failure (Success x)) = Success . f $ Failure x
	multiply f (Success (Success y)) = Success . f $ Success y
	multiply _ (Failure (Failure e)) = Failure e
	multiply _ (Success (Failure e)) = Failure e

instance Alternative (Conclusion e) where
	Failure _ <+> x = x
	Success x <+> _ = Success x

instance Traversable (Conclusion e) where
	Failure y ->> _ = point $ Failure y
	Success x ->> f = Success <$> f x

instance Traversable_ (Conclusion e) (->) (->) where
	_ -<<-- Failure y = point $ Failure y
	f -<<-- Success x = Success -<$>- f x

instance Bindable (Conclusion e) where
	Success x >>= f = f x
	Failure y >>= _ = Failure y

instance Bindable_ (Conclusion e) (->) where
	join_ (Success (Success x)) = Success x
	join_ (Success (Failure y)) = Failure y
	join_ (Failure y) = Failure y

instance Monad (Conclusion e) where

instance Bivariant Conclusion where
	f <-> g = conclusion # Failure . f # Success . g

instance Bivariant_ Conclusion (->) (->) (->) where
	f -<->- g = conclusion # Failure . f # Success . g

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
	run = identity
	unite = identity

type instance Schematic Monad (Conclusion e) = (<.:>) (Conclusion e)

instance Monadic (Conclusion e) where
	wrap = TM . UT . point

type Failable e = Adaptable (Conclusion e)

failure :: Failable e t => e -> t a
failure = adapt . Failure

class Catchable e t where
	catch :: t a -> (e -> t a) -> t a

instance Catchable e (Conclusion e) where
	catch (Failure e) handle = handle e
	catch (Success x) _ = Success x

instance Monad u => Catchable e (Conclusion e <.:> u) where
	catch (UT x) handle = let conclude = conclusion # run . handle # point . Success
		in UT $ x >>= conclude
