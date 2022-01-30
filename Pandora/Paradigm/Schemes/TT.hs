{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Schemes.TT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Betwixt (Betwixt)
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)), (<<-<<-))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<), (--<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (!), (=#-)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Sum ((:+:), bitraverse_sum)
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (empty, point, extract, (<-|-<-|-))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))

newtype TT ct ct' t t' a = TT (t :. t' := a)

infixr 3 <::>, >::>, <::<, >::<

type (<::>) = TT Covariant Covariant
type (>::>) = TT Contravariant Covariant
type (<::<) = TT Covariant Contravariant
type (>::<) = TT Contravariant Contravariant

instance Interpreted (->) (TT ct ct' t t') where
	type Primary (TT ct ct' t t') a = t :. t' := a
	run ~(TT x) = x
	unite = TT

instance (Semigroupoid m, Covariant m m t, Covariant (Betwixt m m) m t, Covariant m (Betwixt m m) t', Interpreted m (t <::> t')) => Covariant m m (t <::> t') where
	(<-|-) f = (=#-) ((<-|-|-) f)

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) t') => Semimonoidal (-->) (:*:) (:*:) (t <::> t') where
	mult = Straight ! TT . (<-|-) (mult @(-->) !) . (mult @(-->) !) . (run :*: run <-|-<-|-)

instance (Covariant (->) (->) t, Covariant (->) (->) t', Semimonoidal (-->) (:*:) (:*:) t', Monoidal (-->) (-->) (:*:) (:*:) t, Monoidal (-->) (-->) (:*:) (:*:) t') => Monoidal (-->) (-->) (:*:) (:*:) (t <::> t') where
	unit _ = Straight ! TT . point . point . (! One) . run

instance (Covariant (->) (->) t, Covariant (->) (->) t', Semimonoidal (-->) (:*:) (:+:) t) => Semimonoidal (-->) (:*:) (:+:) (t <::> t') where
	mult = Straight ! \(TT x :*: TT y) -> TT ! bitraverse_sum identity identity <-|- (mult @(-->) @(:*:) @(:+:) ! (x :*: y))

instance (Covariant (->) (->) t, Covariant (->) (->) t', Semimonoidal (-->) (:*:) (:+:) t, Monoidal (-->) (-->) (:*:) (:+:) t) => Monoidal (-->) (-->) (:*:) (:+:) (t <::> t') where
	unit _ = Straight ! \_ -> TT empty

instance (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) t') => Semimonoidal (<--) (:*:) (:*:) (t <::> t') where
	mult = Flip ! \(TT xys) -> (TT :*: TT <-|-<-|-) . (mult @(<--) !) ! (mult @(<--) !) <-|- xys

instance (Covariant (->) (->) t, Monoidal (<--) (-->) (:*:) (:*:) t, Monoidal (<--) (-->) (:*:) (:*:) t') => Monoidal (<--) (-->) (:*:) (:*:) (t <::> t') where
	unit _ = Flip ! \(TT x) -> Straight (\_ -> extract ! extract x)

instance (Traversable (->) (->) t, Traversable (->) (->) t') => Traversable (->) (->) (t <::> t') where
	f <<- x = TT <-|-- f <<-<<- run x

instance (Bindable (->) t, Distributive (->) (->) t, Covariant (->) (->) t', Bindable (->) t') => Bindable (->) (t <::> t') where
	f =<< TT x = TT ! (\i -> (identity =<<) <-|-- run . f --<< i) =<< x

instance Monoidal (-->) (-->) (:*:) (:*:) t => Liftable (->) (TT Covariant Covariant t) where
	lift :: Covariant (->) (->) t' => t' ~> t <::> t'
	lift = TT . point

instance Monoidal (<--) (-->) (:*:) (:*:) t => Lowerable (->) (TT Covariant Covariant t) where
	lower :: t <::> t' ~> t'
	lower (TT x) = extract x

instance Covariant (->) (->) t => Hoistable (->) (TT Covariant Covariant t) where
	(/|\) :: t' ~> v -> (t <::> t' ~> t <::> v)
	f /|\ TT x = TT ! f <-|- x
