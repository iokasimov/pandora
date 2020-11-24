module Pandora.Paradigm.Schemes.U_T where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Paradigm.Primary.Functor.Product (type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype U_T ct cu t u a = U_T (u a :*: t a)

type (<.:.>) = U_T Covariant Covariant
type (>.:.>) = U_T Contravariant Covariant
type (<.:.<) = U_T Covariant Contravariant
type (>.:.<) = U_T Contravariant Contravariant

instance Interpreted (U_T ct cu t u) where
	type Primary (U_T ct cu t u) a = u a :*: t a
	run ~(U_T x) = x
