module Pandora.Paradigm.Schemes.T_U where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Paradigm.Primary.Functor.Product (type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype T_U ct cu t u a = T_U (t a :*: u a)

type (<:.:>) = T_U Covariant Covariant
type (>:.:>) = T_U Contravariant Covariant
type (<:.:<) = T_U Covariant Contravariant
type (>:.:<) = T_U Contravariant Contravariant

instance Interpreted (T_U ct cu t u) where
	type Primary (T_U ct cu t u) a = t a :*: u a
	run ~(T_U x) = x
