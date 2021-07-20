module Pandora.Paradigm.Schemes.UT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Category ((.), ($), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)), Covariant_ ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)), Applicative_)
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Bindable (Bindable_ ((-=<<-)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))

newtype UT ct cu t u a = UT (u :. t := a)

infixr 3 <.:>, >.:>, <.:<, >.:<

type (<.:>) = UT Covariant Covariant
type (>.:>) = UT Contravariant Covariant
type (<.:<) = UT Covariant Contravariant
type (>.:<) = UT Contravariant Contravariant

instance Interpreted (UT ct cu t u) where
	type Primary (UT ct cu t u) a = u :. t := a
	run ~(UT x) = x
	unite = UT

instance (Covariant t, Covariant u) => Covariant (t <.:> u) where
	f <$> UT x = UT $ f <$$> x

instance (Covariant_ t (->) (->), Covariant_ u (->) (->)) => Covariant_ (t <.:> u) (->) (->) where
	f -<$>- UT x = UT $ f -<$$>- x

instance (Applicative t, Applicative u) => Applicative (t <.:> u) where
	UT f <*> UT x = UT $ f <**> x

instance (Pointable t (->), Pointable u (->)) => Pointable (t <.:> u) (->) where
	point = UT . point . point

instance (Traversable t (->) (->), Bindable_ t (->), Applicative_ u (:*:) (->) (->), Pointable u (->), Bindable_ u (->)) => Bindable_ (t <.:> u) (->) where
	f -=<<- UT x = UT $ ((identity -=<<-) -<$>-) . (run . f <<-) -=<<- x

instance (Extractable t (->), Extractable u (->)) => Extractable (t <.:> u) (->) where
	extract = extract . extract . run

instance Pointable t (->) => Liftable (UT Covariant Covariant t) where
	lift :: Covariant_ u (->) (->) => u ~> t <.:> u
	lift x = UT $ point @_ @(->) -<$>- x

instance Extractable t (->) => Lowerable (UT Covariant Covariant t) where
	lower :: Covariant_ u (->) (->) => t <.:> u ~> u
	lower (UT x) = extract @_ @(->) -<$>- x
