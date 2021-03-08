module Pandora.Paradigm.Schemes.UT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=), join))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

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

instance (Applicative t, Applicative u) => Applicative (t <.:> u) where
	UT f <*> UT x = UT $ f <**> x

instance (Pointable t, Pointable u) => Pointable (t <.:> u) where
	point = UT . point . point

instance (Traversable t, Bindable t, Applicative u, Monad u) => Bindable (t <.:> u) where
	UT x >>= f = UT $ x >>= \i -> join <$> i ->> run . f

instance (Extractable t, Extractable u) => Extractable (t <.:> u) where
	extract = extract . extract . run

instance Pointable t => Liftable (UT Covariant Covariant t) where
	lift :: Covariant u => u ~> t <.:> u
	lift x = UT $ point <$> x

instance Extractable t => Lowerable (UT Covariant Covariant t) where
	lower :: Covariant u => t <.:> u ~> u
	lower (UT x) = extract <$> x
