module Pandora.Paradigm.Schemes.U_T where

import Pandora.Paradigm.Primary.Functor.Product (type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype U_T ct cu t u a = U_T (u a :*: t a)

instance Interpreted (U_T ct cu t u) where
	type Primary (U_T ct cu t u) a = u a :*: t a
	run ~(U_T x) = x
