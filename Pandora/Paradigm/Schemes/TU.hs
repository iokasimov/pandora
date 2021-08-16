module Pandora.Paradigm.Schemes.TU where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), identity)
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply_))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)), (-<<-<<-))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Sum ((:+:) (Option, Adoption), sum)
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (empty, point, extract_)

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

instance (Covariant t (->) (->), Covariant u (->) (->)) => Covariant (t <:.> u) (->) (->) where
	f -<$>- x = TU $ f -<$$>- run x

instance (Covariant t (->) (->), Semimonoidal t (->) (:*:) (:*:), Semimonoidal u (->) (:*:) (:*:)) => Semimonoidal (t <:.> u) (->) (:*:) (:*:) where
	multiply_ (TU x :*: TU y) = TU $ multiply_ @_ @(->) @(:*:) -<$>- multiply_ (x :*: y)

instance (Covariant t (->) (->), Covariant u (->) (->), Semimonoidal t (->) (:*:) (:+:)) => Semimonoidal (t <:.> u) (->) (:*:) (:+:) where
	multiply_ (TU x :*: TU y) = TU $ sum (Option -<$>-) (Adoption -<$>-) -<$>- multiply_ @_ @(->) @(:*:) @(:+:) (x :*: y)

instance (Covariant t (->) (->), Covariant u (->) (->), Semimonoidal u (->) (:*:) (:*:), Monoidal t (->) (->) (:*:) (:*:), Monoidal u (->) (->) (:*:) (:*:)) => Monoidal (t <:.> u) (->) (->) (:*:) (:*:) where
	unit _ f = TU . point . point $ f One

instance (Covariant t (->) (->), Covariant u (->) (->), Monoidal t (->) (->) (:*:) (:+:)) => Monoidal (t <:.> u) (->) (->) (:*:) (:+:) where
	unit _ _ = TU empty

instance (Extractable t (->), Extractable u (->)) => Extractable (t <:.> u) (->) where
	extract = extract . extract . run

instance (Traversable t (->) (->), Traversable u (->) (->)) => Traversable (t <:.> u) (->) (->) where
	f <<- x = TU -<$>- f -<<-<<- run x

instance (Bindable t (->), Distributive t (->) (->), Covariant u (->) (->), Bindable u (->)) => Bindable (t <:.> u) (->) where
	f =<< TU x = TU $ (\i -> (identity =<<) -<$>- run . f -<< i) =<< x

instance Monoidal t (->) (->) (:*:) (:*:) => Liftable (TU Covariant Covariant t) where
	lift :: Covariant u (->) (->) => u ~> t <:.> u
	lift = TU . point

instance Monoidal t (<--) (->) (:*:) (:*:) => Lowerable (TU Covariant Covariant t) where
	lower :: t <:.> u ~> u
	lower (TU x) = extract_ x

instance Covariant t (->) (->) => Hoistable (TU Covariant Covariant t) where
	(/|\) :: u ~> v -> (t <:.> u ~> t <:.> v)
	f /|\ TU x = TU $ f -<$>- x
