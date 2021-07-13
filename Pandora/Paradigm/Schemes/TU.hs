module Pandora.Paradigm.Schemes.TU where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)), Covariant_ ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)), Traversable_ ((-<<--)), (-<<-<<-))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=), join))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype TU ct cu t u a = TU (t :. u := a)

infixr 3 <:.>, >:.>, <:.<, >:.<

type (<:.>) = TU Covariant Covariant
type (>:.>) = TU Contravariant Covariant
type (<:.<) = TU Covariant Contravariant
type (>:.<) = TU Contravariant Contravariant

instance Interpreted (TU ct cu t u) where
	type Primary (TU ct cu t u) a = t :. u := a
	run ~(TU x) = x
	unite = TU

instance (Covariant t, Covariant u) => Covariant (t <:.> u) where
	f <$> x = TU $ f <$$> run x

instance (Covariant_ t (->) (->), Covariant_ u (->) (->)) => Covariant_ (t <:.> u) (->) (->) where
	f -<$>- x = TU $ f -<$$>- run x

instance (Applicative t, Applicative u) => Applicative (t <:.> u) where
	TU f <*> TU x = TU $ f <**> x

instance (Covariant u, Alternative t) => Alternative (t <:.> u) where
	x <+> y = TU $ run x <+> run y

instance (Covariant u, Avoidable t) => Avoidable (t <:.> u) where
	empty = TU empty

instance (Pointable t (->), Pointable u (->)) => Pointable (t <:.> u) (->) where
	point = TU . point . point

instance (Extractable t (->), Extractable u (->)) => Extractable (t <:.> u) (->) where
	extract = extract . extract . run

instance (Traversable t, Traversable u) => Traversable (t <:.> u) where
	x ->> f = TU <$> run x ->>> f

instance (Traversable_ t (->) (->), Traversable_ u (->) (->)) => Traversable_ (t <:.> u) (->) (->) where
	f -<<-- x = TU -<$>- f -<<-<<- run x

instance (Bindable t, Distributive t, Covariant_ u (->) (->), Bindable u) => Bindable (t <:.> u) where
	TU x >>= f = TU $ x >>= \i -> join -<$>- i >>- run . f

instance Pointable t (->) => Liftable (TU Covariant Covariant t) where
	lift :: Covariant_ u (->) (->) => u ~> t <:.> u
	lift = TU . point

instance Extractable t (->) => Lowerable (TU Covariant Covariant t) where
	lower :: t <:.> u ~> u
	lower (TU x) = extract x

instance Covariant_ t (->) (->) => Hoistable (TU Covariant Covariant t) where
	(/|\) :: u ~> v -> (t <:.> u ~> t <:.> v)
	f /|\ TU x = TU $ f -<$>- x
