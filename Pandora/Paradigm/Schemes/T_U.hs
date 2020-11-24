module Pandora.Paradigm.Schemes.T_U where

import Pandora.Paradigm.Primary.Functor.Product (type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype T_U ct cu t u a = T_U (t a :*: u a)

instance Interpreted (T_U ct cu t u) where
	type Primary (T_U ct cu t u) a = t a :*: u a
	run ~(T_U x) = x
