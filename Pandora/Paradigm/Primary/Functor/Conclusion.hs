module Pandora.Paradigm.Primary.Functor.Conclusion where

import Pandora.Core.Functor (type (~>))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~)))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Morphism.Kleisli (Kleisli (Kleisli))
import Pandora.Pattern.Category (identity, (<--), (<---), (<----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor (Functor ((-|-)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Pattern.Operation.Exponential (type (-->))
import Pandora.Pattern.Operation.One (One (One))
import Pandora.Paradigm.Algebraic (point)
import Pandora.Paradigm.Schemes (Schematic, UT (UT), type (<.:>))

-- TODO: rename it to Progress = Stop e | Continue a
-- it would be a more generalized and semantic-based name
data Conclusion e a = Failure e | Success a

instance Functor (-->) (-->) (Conclusion e) where
	(-|-) (Straight f) = Straight <-- \case
		Success x -> Success <-- f x
		Failure y -> Failure y

instance Functor (Kleisli (Conclusion e) (->)) (-->) (Conclusion e) where
	(-|-) (Kleisli f) = Straight <-- \case
		Failure e -> Failure e
		Success x -> f x

instance Covariant (->) (->) (Conclusion e) where
	f <-|- Success x = Success <-- f x
	_ <-|- Failure y = Failure y

instance Semimonoidal (-->) (:*:) (:*:) (Conclusion e) where
	mult = Straight <-- \case
		Success x :*: Success y -> Success <--- x :*: y
		Failure x :*: _ -> Failure x
		_ :*: Failure x -> Failure x

instance Monoidal (-->) (-->) (:*:) (:*:) (Conclusion e) where
	unit _ = Straight <--- Success . (<~ One)

instance Semigroup e => Semimonoidal (-->) (:*:) (:+:) (Conclusion e) where
	mult = Straight <-- \case
		Failure _ :*: x -> Adoption <-|- x
		Success x :*: _ -> Option <-|- Success x

instance Traversable (->) (->) (Conclusion e) where
	(<-/-) :: (Covariant (->) (->) u, Monoidal (-->) (-->) (:*:) (:*:) u, Semimonoidal (-->) (:*:) (:*:) u)
		 => (a -> u b) -> Conclusion e a -> u (Conclusion e b)
	_ <-/- Failure y = point <-- Failure y
	f <-/- Success x = Success <-|- f x

instance Bindable (->) (Conclusion e) where
	f =<< Success x = f x
	_ =<< Failure y = Failure y

--instance Monad (Conclusion e) where

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
	Success x + Success y = Success <-- x + y
	Failure x + Failure y = Failure <-- x + y
	Failure _ + Success y = Success y
	Success x + Failure _ = Success x

conclusion :: (e -> r) -> (a -> r) -> Conclusion e a -> r
conclusion f _ (Failure x) = f x
conclusion _ s (Success x) = s x

fail :: (e -> r) -> Conclusion e ~> Conclusion r
fail f (Failure x) = Failure <-- f x
fail _ (Success y) = Success y

instance Interpreted (->) (Conclusion e) where
	type Primary (Conclusion e) a = Conclusion e a
	run = identity
	unite = identity

type instance Schematic Monad (Conclusion e) = (<.:>) (Conclusion e)

instance Monadic (->) (Conclusion e) where
	wrap = TM . UT . point

type Failable e t = Adaptable t (->) (Conclusion e)

failure :: Failable e t => e -> t a
failure = adapt . Failure

class Catchable e t where
	catch :: t a -> (e -> t a) -> t a

instance Catchable e (Conclusion e) where
	catch (Failure e) handle = handle e
	catch (Success x) _ = Success x

instance (Monoidal (-->) (-->) (:*:) (:*:) u, Bindable (->) u) => Catchable e (Conclusion e <.:> u) where
	catch (UT x) handle = let conclude = conclusion <-- run . handle <-- point . Success
		in UT <--- conclude =<< x

instance Covariant (->) (->) (Flip Conclusion a) where
	_ <-|- Flip (Success x) = Flip <-- Success x
	f <-|- Flip (Failure y) = Flip . Failure <-- f y

instance Semimonoidal (-->) (:*:) (:*:) (Flip Conclusion a) where
	mult = Straight <-- \case
		Flip (Failure x) :*: Flip (Failure y) -> Flip <---- Failure <--- x :*: y
		Flip (Success x) :*: _ -> Flip <-- Success x
		_ :*: Flip (Success x) -> Flip <-- Success x

instance Monoidal (-->) (-->) (:*:) (:*:) (Flip Conclusion a) where
	unit _ = Straight <--- Flip . Failure . (<~ One)

instance Traversable (->) (->) (Flip Conclusion a) where
	(<-/-) :: (Covariant (->) (->) u, Monoidal (-->) (-->) (:*:) (:*:) u, Semimonoidal (-->) (:*:) (:*:) u)
		 => (e -> u e') -> Flip Conclusion a e -> u (Flip Conclusion a e')
	_ <-/- Flip (Success y) = point <--- Flip <-- Success y
	f <-/- Flip (Failure x) = Flip . Failure <-|- f x
