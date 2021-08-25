module Pandora.Paradigm.Schemes.TUT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, ($))
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((-<$>-)), (-<$$>-), (-<$$$>-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point, extract)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
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

instance (Covariant (->) (->) t, Covariant (->) (->) t', Covariant (->) (->) u) => Covariant (->) (->) (t <:<.>:> t' := u) where
	f -<$>- TUT x = TUT $ f -<$$$>- x

instance (Covariant (->) (->) t, Covariant (->) (->) t', Covariant (->) (->) u, Semimonoidal (->) (:*:) (:*:) t, Semimonoidal (->) (:*:) (:*:) u, Semimonoidal (->) (:*:) (:*:) t') => Semimonoidal (->) (:*:) (:*:) (t <:<.>:> t' := u) where
	multiply (TUT x :*: TUT y) = TUT $ multiply @(->) @(:*:) -<$$>- multiply @(->) @(:*:) -<$>- multiply (x :*: y)

instance (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t, Covariant (->) (->) u, Semimonoidal (<--) (:*:) (:*:) u, Covariant (->) (->) t', Semimonoidal (<--) (:*:) (:*:) t') => Semimonoidal (<--) (:*:) (:*:) (t <:<.>:> t' := u) where
	multiply = Flip $ \(TUT xys) ->
		let Flip f = multiply @(<--) @(:*:) @(:*:) in
		let Flip g = multiply @(<--) @(:*:) @(:*:) in
		let Flip h = multiply @(<--) @(:*:) @(:*:) in
		(TUT <-> TUT) $ f (g -<$>- (h -<$$>- xys)) where

instance (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) t', Monoidal u (<--) (->) (:*:) (:*:), Adjoint (->) (->) t t') => Monoidal (t <:<.>:> t' := u) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(TUT xys) -> (\_ -> (extract |-) xys)

instance (Covariant (->) (->) t, Covariant (->) (->) t', Adjoint (->) (->) t' t, Bindable (->) u) => Bindable (->) (t <:<.>:> t' := u) where
	f =<< x = TUT $ ((run . f |-) =<<) -<$>- run x

instance (Covariant (->) (->) t, Covariant (->) (->) u, Covariant (->) (->) t', Semimonoidal (->) (:*:) (:*:) t, Semimonoidal (->) (:*:) (:*:) t', Monoidal u (->) (->) (:*:) (:*:), Adjoint (->) (->) t' t) => Monoidal (t <:<.>:> t' := u) (->) (->) (:*:) (:*:) where
	unit _ f = unite . (point -|) . f $ One

instance (Adjoint (->) (->) t' t, Extendable (->) u) => Extendable (->) (t' <:<.>:> t := u) where
	f <<= x = TUT $ ((f . unite -|) <<=) -<$>- run x

instance (Adjoint (->) (->) t' t, Distributive (->) (->) t) => Liftable (t <:<.>:> t') where
	lift :: Covariant (->) (->) u => u ~> t <:<.>:> t' := u
	lift x = TUT $ (identity @(->) -|) -<< x

instance (Adjoint (->) (->) t t', Distributive (->) (->) t') => Lowerable (t <:<.>:> t') where
	lower :: Covariant (->) (->) u => (t <:<.>:> t' := u) ~> u
	lower (TUT x) = (identity @(->) -<<) |- x
