module Pandora.Paradigm.Schemes.TUT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor ((<*+>))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$$>)), Covariant_ ((-<$>-)), (-<$$$>-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=), ($>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>), ($=>>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
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

instance (Covariant t, Covariant t', Covariant u) => Covariant (t <:<.>:> t' := u) where
	f <$> TUT x = TUT $ f <$$$> x

instance (Covariant_ t (->) (->), Covariant_ t' (->) (->), Covariant_ u (->) (->)) => Covariant_ (t <:<.>:> t' := u) (->) (->)where
	f -<$>- TUT x = TUT $ f -<$$$>- x

instance (Covariant t, Covariant t', Adjoint t' t, Bindable u) => Applicative (t <:<.>:> t' := u) where
	f <*> x = TUT $ (>>= (|- (<$$$> run x))) <$> run f

instance (Applicative t, Covariant t', Alternative u) => Alternative (t <:<.>:> t' := u) where
	x <+> y = TUT $ run x <*+> run y

instance (Pointable t (->), Applicative t, Covariant t', Avoidable u) => Avoidable (t <:<.>:> t' := u) where
	empty = TUT $ point empty

instance (Covariant_ t (->) (->), Covariant_ t' (->) (->), Pointable u (->), Adjoint t' t) => Pointable (t <:<.>:> t' := u) (->) where
	point = unite . (-| point)

instance (Covariant t, Covariant t', Adjoint t' t, Bindable u) => Bindable (t <:<.>:> t' := u) where
	x >>= f = TUT $ run x $>>= (|- run . f)

instance (Covariant t', Covariant t, Adjoint t' t, Extendable u) => Extendable (t' <:<.>:> t := u) where
	x =>> f = TUT $ run x $=>> (-| f . unite)

instance (Covariant_ t (->) (->), Covariant_ t' (->) (->), Adjoint t t', Extractable u (->)) => Extractable (t <:<.>:> t' := u) (->) where
	extract = (|- extract) . run

instance (forall u . Covariant u, Adjoint t' t, Distributive t) => Liftable (t <:<.>:> t') where
	lift :: Covariant_ u (->) (->) => u ~> t <:<.>:> t' := u
	lift x = TUT $ x >>- (-| identity)

instance (forall u . Covariant u, Adjoint t t', Distributive t') => Lowerable (t <:<.>:> t') where
	lower :: Covariant_ u (->) (->) => (t <:<.>:> t' := u) ~> u
	lower (TUT x) = x |- (>>- identity)
