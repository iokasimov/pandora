{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Schemes.TUT where

import Pandora.Core.Functor (type (:.), type (>), type (>>>), type (>>>>>>>>), type (~>))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~), (<~~~), (=#-)))
import Pandora.Pattern.Betwixt (Betwixt)
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (identity, (<--), (<---), (<----), (<------))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((<-|-), (<-|--), (<-|---), (<-|-|-), (<-|-|---), (<-|-|-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (--|), (|-), (|--)))
import Pandora.Pattern.Transformation.Liftable (Liftable (lift))
import Pandora.Pattern.Transformation.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Algebraic.Exponential (type (--<), type (-->))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Pattern.Operation.One (One (One))
import Pandora.Paradigm.Algebraic (point, extract, empty, (<<-|-))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))

newtype TUT ct ct' cu t t' u a = TUT (t :. u :. t' >>> a)

infix 3 <:<.>:>, >:<.>:>, <:<.>:<, >:<.>:<, <:>.<:>, >:>.<:>, <:>.<:<, >:>.<:<

type (<:<.>:>) = TUT Covariant Covariant Covariant
type (>:<.>:>) = TUT Contravariant Covariant Covariant
type (<:<.>:<) = TUT Covariant Covariant Contravariant
type (>:<.>:<) = TUT Contravariant Covariant Contravariant
type (<:>.<:>) = TUT Covariant Contravariant Covariant
type (>:>.<:>) = TUT Contravariant Contravariant Covariant
type (<:>.<:<) = TUT Covariant Contravariant Contravariant
type (>:>.<:<) = TUT Contravariant Contravariant Contravariant

instance Interpreted (->) (TUT ct ct' cu t t' u) where
	type Primary (TUT ct ct' cu t t' u) a = t :. u :. t' >>> a
	run ~(TUT x) = x
	unite = TUT

instance (Semigroupoid m, Covariant m m t, Covariant (Betwixt (Betwixt m m) m) m t, Covariant (Betwixt m (Betwixt m m)) (Betwixt (Betwixt m m) m) u, Covariant m (Betwixt m (Betwixt m m)) t', Interpreted m (t <:<.>:> t' >>>>>>>> u)) => Covariant m m (t <:<.>:> t' >>>>>>>> u) where
	(<-|-) f = (=#-) ((<-|-|-|-) f)

instance (Adjoint (->) (->) t' t, Bindable (->) u) => Semimonoidal (-->) (:*:) (:*:) (t <:<.>:> t' >>>>>>>> u) where
	mult = Straight <-- \(TUT x :*: TUT y) -> TUT ((((\r -> (<-|-|-|-) (r :*:) y) |-) =<<) <-|- x)

instance (Covariant (->) (->) t, Semimonoidal (--<) (:*:) (:*:) t, Covariant (->) (->) u, Semimonoidal (--<) (:*:) (:*:) u, Covariant (->) (->) t', Semimonoidal (--<) (:*:) (:*:) t') => Semimonoidal (--<) (:*:) (:*:) (t <:<.>:> t' >>>>>>>> u) where
	mult = Flip <-- (TUT <<-|-) . (TUT <-|-) . (mult @(--<) <~) . (<-|-) (mult @(--<) <~) . (<-|-|-) @_ @(->) (mult @(--<) <~) . run

instance (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (--<) (:*:) (:*:) t, Semimonoidal (--<) (:*:) (:*:) t', Monoidal (--<) (-->) (:*:) (:*:) u, Adjoint (->) (->) t t') => Monoidal (--<) (-->) (:*:) (:*:) (t <:<.>:> t' >>>>>>>> u) where
	unit _ = Flip <-- \(TUT xys) -> Straight (\_ -> (extract |-) xys)

-- TODO: generalize on (->) and (:*:)
instance {-# OVERLAPS #-} (Covariant (->) (->) u, Semimonoidal (-->) (:*:) (:+:) u) 
	=> Semimonoidal (-->) (:*:) (:+:) ((->) s <:<.>:> (:*:) s >>>>>>>> u) where
	mult = Straight <-- \(TUT x :*: TUT y) -> TUT
		<------ product_over_sum
			<-|-|- mult @(-->) @(:*:) @(:+:)
				<-|-- mult @(-->) @(:*:) @(:*:)
					<~~~ x :*: y

instance {-# OVERLAPS #-} (Covariant (->) (->) u, Monoidal (-->) (-->) (:*:) (:+:) u) 
	=> Monoidal (-->) (-->) (:*:) (:+:) ((->) s <:<.>:> (:*:) s >>>>>>>> u) where
	unit _ = Straight <-- \_ -> empty

product_over_sum :: (s :*: a) :+: (s :*: b) -> s :*: (a :+: b)
product_over_sum (Option (s :*: x)) = s :*: Option x
product_over_sum (Adoption (s :*: y)) = s :*: Adoption y

instance (Covariant (->) (->) t, Covariant (->) (->) t', Adjoint (->) (->) t' t, Bindable (->) u) => Bindable (->) (t <:<.>:> t' >>>>>>>> u) where
	f =<< x = TUT <---- ((run . f |--) =<<) <-|- run x

instance (Bindable (->) u, Monoidal (-->) (-->) (:*:) (:*:) u, Adjoint (->) (->) t' t) => Monoidal (-->) (-->) (:*:) (:*:) (t <:<.>:> t' >>>>>>>> u) where
	unit _ = Straight <-- unite . (point -|) . (<~ One)

instance (Adjoint (->) (->) t' t, Extendable (->) u) => Extendable (->) (t' <:<.>:> t >>>>>>>> u) where
	f <<= x = TUT <---- ((f . unite --|) <<=) <-|- run x

instance (Adjoint (->) (->) t' t, Distributive (->) (->) t) => Liftable (->) (t <:<.>:> t') where
	lift :: Covariant (->) (->) u => u ~> t <:<.>:> t' >>>>>>>> u
	lift x = TUT <--- (identity @(->) -|) -<< x

instance (Adjoint (->) (->) t t', Distributive (->) (->) t') => Lowerable (->) (t <:<.>:> t') where
	lower :: Covariant (->) (->) u => (t <:<.>:> t' >>>>>>>> u) ~> u
	lower (TUT x) = (identity @(->) -<<) |- x
