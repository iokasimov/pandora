{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Primary.Transformer.Reverse where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-), (<-/--)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<), (--<<)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Transformer.Backwards (Backwards (Backwards))
import Pandora.Paradigm.Algebraic.Exponential (type (<--), type (-->))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:))
import Pandora.Paradigm.Algebraic.One (One (One))
import Pandora.Paradigm.Algebraic (point, extract, empty, (<-||-))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~), (<~~~)))

newtype Reverse t a = Reverse (t a)

instance Covariant (->) (->) t => Covariant (->) (->) (Reverse t) where
	f <-|- Reverse x = Reverse <---- f <-|- x

instance (Semimonoidal (-->) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (-->) (:*:) (:*:) (Reverse t) where
	mult = Straight <-- \(Reverse x :*: Reverse y) -> Reverse <---- mult @(-->) <~~~ x :*: y

instance (Covariant (->) (->) t, Monoidal (-->) (-->) (:*:) (:*:) t) => Monoidal (-->) (-->) (:*:) (:*:) (Reverse t) where
	unit _ = Straight <-- Reverse . point . (<~ One)

instance (Semimonoidal (<--) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (<--) (:*:) (:*:) (Reverse t) where
	mult = Flip <-- (Reverse <-||-) . (Reverse <-|-) . (mult @(<--) <~) . run

instance (Covariant (->) (->) t, Monoidal (<--) (-->) (:*:) (:*:) t) => Monoidal (<--) (-->) (:*:) (:*:) (Reverse t) where
	unit _ = Flip <-- \(Reverse x) -> Straight (\_ -> extract x)

instance (Semimonoidal (-->) (:*:) (:+:) t, Covariant (->) (->) t) => Semimonoidal (-->) (:*:) (:+:) (Reverse t) where
	mult = Straight <-- \(Reverse x :*: Reverse y) -> Reverse <---- mult @(-->)  @(:*:) @(:+:) <~~~ x :*: y

instance (Covariant (->) (->) t, Monoidal (-->) (-->) (:*:) (:+:) t) => Monoidal (-->) (-->) (:*:) (:+:) (Reverse t) where
	unit _ = Straight <-- \_ -> Reverse empty

instance Traversable (->) (->) t => Traversable (->) (->) (Reverse t) where
	f <-/- Reverse x = Reverse <-|- run (Backwards . f <-/-- x)

instance Distributive (->) (->) t => Distributive (->) (->) (Reverse t) where
	f -<< x = Reverse <--- run . f -<< x

instance Contravariant (->) (->) t => Contravariant (->) (->) (Reverse t) where
	f >-|- Reverse x = Reverse <---- f >-|- x

instance Interpreted (->) (Reverse t) where
	type Primary (Reverse t) a = t a
	run ~(Reverse x) = x
	unite = Reverse

instance Liftable (->) Reverse where
	lift = Reverse

instance Lowerable (->) Reverse where
	lower = run

instance Hoistable (->) Reverse where
	f /|\ Reverse x = Reverse <-- f x
