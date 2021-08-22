module Pandora.Paradigm.Schemes.UT where

import Pandora.Core.Functor (type (:.), type (:=), type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), identity)
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic ((:*:) ((:*:)), point, extract)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))

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

instance (Covariant (->) (->) t, Covariant (->) (->) u) => Covariant (->) (->) (t <.:> u) where
	f -<$>- x = UT $ f -<$$>- run x

instance (Covariant (->) (->) u, Semimonoidal (->) (:*:) (:*:) t, Semimonoidal (->) (:*:) (:*:) u) => Semimonoidal (->) (:*:) (:*:) (t <.:> u) where
	multiply (UT x :*: UT y) = UT $ multiply @(->) @(:*:) -<$>- multiply (x :*: y)

instance (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (->) (:*:) (:*:) u, Monoidal t (->) (->) (:*:) (:*:), Monoidal u (->) (->) (:*:) (:*:)) => Monoidal (t <.:> u) (->) (->) (:*:) (:*:) where
	unit _ f = UT . point . point $ f One

instance (Traversable t (->) (->), Bindable t (->), Semimonoidal (->) (:*:) (:*:) u, Monoidal u (->) (->) (:*:) (:*:), Bindable u (->)) => Bindable (t <.:> u) (->) where
	f =<< UT x = UT $ ((identity =<<) -<$>-) . (run . f <<-) =<< x

instance (Covariant (->) (->) u, Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) u) => Semimonoidal (<--) (:*:) (:*:) (t <.:> u) where
	multiply = Flip $ \(UT xys) ->
		let Flip f = multiply @(<--) @(:*:) @(:*:) in
		let Flip g = multiply @(<--) @(:*:) @(:*:) in
		(UT <-> UT) $ f (g -<$>- xys) where

instance (Covariant (->) (->) u, Monoidal t (<--) (->) (:*:) (:*:), Monoidal u (<--) (->) (:*:) (:*:)) => Monoidal (t <.:> u) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(UT x) -> (\_ -> extract $ extract x)

instance Monoidal t (->) (->) (:*:) (:*:) => Liftable (UT Covariant Covariant t) where
	lift :: Covariant (->) (->) u => u ~> t <.:> u
	lift x = UT $ point -<$>- x

instance Monoidal t (<--) (->) (:*:) (:*:) => Lowerable (UT Covariant Covariant t) where
	lower :: Covariant (->) (->) u => t <.:> u ~> u
	lower (UT x) = extract -<$>- x
