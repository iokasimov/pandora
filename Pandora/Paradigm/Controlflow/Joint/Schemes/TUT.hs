module Pandora.Paradigm.Controlflow.Joint.Schemes.TUT (TUT (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype TUT ct ct' cu t t' u a = TUT (t :. u :. t' := a)

instance Interpreted (TUT ct ct' cu t t' u) where
	type Primary (TUT ct ct' cu t t' u) a = t :. u :. t' := a
	run (TUT x) = x
