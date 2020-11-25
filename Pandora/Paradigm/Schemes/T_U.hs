module Pandora.Paradigm.Schemes.T_U where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype T_U ct cu t u a = T_U (t a :*: u a)

type (<:.:>) = T_U Covariant Covariant
type (>:.:>) = T_U Contravariant Covariant
type (<:.:<) = T_U Covariant Contravariant
type (>:.:<) = T_U Contravariant Contravariant

instance Interpreted (T_U ct cu t u) where
	type Primary (T_U ct cu t u) a = t a :*: u a
	run ~(T_U x) = x

instance Avoidable t => Liftable (T_U Covariant Covariant t) where
	lift :: Covariant u => u ~> t <:.:> u
	lift x = T_U $ empty :*: x

instance Lowerable (T_U Covariant Covariant t) where
	lower :: t <:.:> u ~> u
	lower (T_U (x :*: y)) = y
