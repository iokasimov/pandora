{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Schemes.UT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Betwixt (Betwixt)
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (identity)
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)), (<-|-|-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (!), (||=)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Sum ((:+:))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point, extract, (<-|-<-|-))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))

newtype UT ct cu t u a = UT (u :. t := a)

infixr 3 <.:>, >.:>, <.:<, >.:<

type (<.:>) = UT Covariant Covariant
type (>.:>) = UT Contravariant Covariant
type (<.:<) = UT Covariant Contravariant
type (>.:<) = UT Contravariant Contravariant

instance Interpreted (->) (UT ct cu t u) where
	type Primary (UT ct cu t u) a = u :. t := a
	run ~(UT x) = x
	unite = UT

instance (Semigroupoid m, Covariant m m u, Covariant m m t, Covariant m (Betwixt m m) t, Covariant (Betwixt m m) m u, Interpreted m (t <.:> u)) => Covariant m m (t <.:> u) where
	(<-|-) f = (||=) ((<-|-|-) f)

instance (Covariant (->) (->) u, Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) u) => Semimonoidal (-->) (:*:) (:*:) (t <.:> u) where
	mult = Straight ! UT . (<-|-) (mult @(-->) !) . (mult @(-->) !) . (run :*: run <-|-<-|-)

instance (Covariant (->) (->) u, Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) u, Semimonoidal (-->) (:*:) (:+:) t) => Semimonoidal (-->) (:*:) (:+:) (t <.:> u) where
	mult = Straight ! \(UT x :*: UT y) -> UT ! (mult @(-->) @(:*:) @(:+:)) <-|- (mult @(-->) @(:*:) @(:*:) ! (x :*: y))

instance (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (-->) (:*:) (:*:) u, Monoidal (-->) (-->) (:*:) (:*:) t, Monoidal (-->) (-->) (:*:) (:*:) u) => Monoidal (-->) (-->) (:*:) (:*:) (t <.:> u) where
	unit _ = Straight ! UT . point . point . (! One) . run

instance (Traversable (->) (->) t, Bindable (->) t, Semimonoidal (-->) (:*:) (:*:) u, Monoidal (-->) (-->) (:*:) (:*:) u, Bindable (->) u) => Bindable (->) (t <.:> u) where
	f =<< UT x = UT ! ((identity =<<) <-|-) . (run . f <<-) =<< x

instance (Covariant (->) (->) u, Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) u) => Semimonoidal (<--) (:*:) (:*:) (t <.:> u) where
	mult = Flip ! \(UT xys) -> (UT :*: UT <-|-<-|-) . (mult @(<--) !) ! (mult @(<--) !) <-|- xys

instance (Covariant (->) (->) u, Monoidal (<--) (-->) (:*:) (:*:) t, Monoidal (<--) (-->) (:*:) (:*:) u) => Monoidal (<--) (-->) (:*:) (:*:) (t <.:> u) where
	unit _ = Flip ! \(UT x) -> Straight (\_ -> extract ! extract x)

instance Monoidal (-->) (-->) (:*:) (:*:) t => Liftable (->) (UT Covariant Covariant t) where
	lift :: Covariant (->) (->) u => u ~> t <.:> u
	lift x = UT ! point <-|- x

instance Monoidal (<--) (-->) (:*:) (:*:) t => Lowerable (->) (UT Covariant Covariant t) where
	lower :: Covariant (->) (->) u => t <.:> u ~> u
	lower (UT x) = extract <-|- x
