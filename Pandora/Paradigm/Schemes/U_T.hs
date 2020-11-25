module Pandora.Paradigm.Schemes.U_T where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype U_T ct cu t u a = U_T (u a :*: t a)

type (<.:.>) = U_T Covariant Covariant
type (>.:.>) = U_T Contravariant Covariant
type (<.:.<) = U_T Covariant Contravariant
type (>.:.<) = U_T Contravariant Contravariant

instance Interpreted (U_T ct cu t u) where
	type Primary (U_T ct cu t u) a = u a :*: t a
	run ~(U_T x) = x

instance Avoidable t => Liftable (U_T Covariant Covariant t) where
	lift :: Covariant u => u ~> t <.:.> u
	lift x = U_T $ x :*: empty

instance Lowerable (U_T Covariant Covariant t) where
	lower :: t <.:.> u ~> u
	lower (U_T (x :*: y)) = x
