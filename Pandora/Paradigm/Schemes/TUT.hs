module Pandora.Paradigm.Schemes.TUT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, ($))
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((-<$>-)), (-<$$>-), (-<$$$>-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply_))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype TUT ct ct' cu t t' u a = TUT (t :. u :. t' := a)

infix 3 <:<.>:>, >:<.>:>, <:<.>:<, >:<.>:<, <:>.<:>, >:>.<:>, <:>.<:<, >:>.<:<

type (<:<.>:>) = TUT Covariant Covariant Covariant
type (>:<.>:>) = TUT Contravariant Covariant Covariant
type (<:<.>:<) = TUT Covariant Covariant Contravariant
type (>:<.>:<) = TUT Contravariant Covariant Contravariant
type (<:>.<:>) = TUT Covariant Contravariant Covariant
type (>:>.<:>) = TUT Contravariant Contravariant Covariant
type (<:>.<:<) = TUT Covariant Contravariant Contravariant
type (>:>.<:<) = TUT Contravariant Contravariant Contravariant

instance Interpreted (TUT ct ct' cu t t' u) where
	type Primary (TUT ct ct' cu t t' u) a = t :. u :. t' := a
	run ~(TUT x) = x
	unite = TUT

instance (Covariant t (->) (->), Covariant t' (->) (->), Covariant u (->) (->)) => Covariant (t <:<.>:> t' := u) (->) (->) where
	f -<$>- TUT x = TUT $ f -<$$$>- x

instance (Covariant t (->) (->), Covariant t' (->) (->), Covariant u (->) (->), Semimonoidal t (->) (:*:) (:*:), Semimonoidal u (->) (:*:) (:*:), Semimonoidal t' (->) (:*:) (:*:)) => Semimonoidal (t <:<.>:> t' := u) (->) (:*:) (:*:) where
	multiply_ (TUT x :*: TUT y) = TUT $ multiply_ @_ @(->) @(:*:) -<$$>- multiply_ @_ @(->) @(:*:) -<$>- multiply_ (x :*: y)

instance (Covariant t (->) (->), Covariant t' (->) (->), Pointable u (->), Adjoint t' t (->) (->)) => Pointable (t <:<.>:> t' := u) (->) where
	point = unite . (point @_ @(->) -|)

instance (Adjoint t' t (->) (->), Extendable u (->)) => Extendable (t' <:<.>:> t := u) (->) where
	f <<= x = TUT $ ((f . unite -|) <<=) -<$>- run x

instance (Covariant t (->) (->), Covariant t' (->) (->), Adjoint t t' (->) (->), Extractable u (->)) => Extractable (t <:<.>:> t' := u) (->) where
	extract = (extract @_ @(->) |-) . run

instance (Adjoint t' t (->) (->), Distributive t(->) (->) ) => Liftable (t <:<.>:> t') where
	lift :: Covariant u (->) (->) => u ~> t <:<.>:> t' := u
	lift x = TUT $ (identity @(->) -|) -<< x

instance (Adjoint t t' (->) (->), Distributive t'(->) (->) ) => Lowerable (t <:<.>:> t') where
	lower :: Covariant u (->) (->) => (t <:<.>:> t' := u) ~> u
	lower (TUT x) = (identity @(->) -<<) |- x
