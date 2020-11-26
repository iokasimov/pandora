module Pandora.Paradigm.Schemes.T_ where

import Pandora.Paradigm.Primary.Functor.Product (type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype T_ ct t a = T_ (a :*: t a)

instance Interpreted (T_ ct t) where
	type Primary (T_ ct t) a = a :*: t a
	run ~(T_ x) = x
