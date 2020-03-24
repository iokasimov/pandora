module Pandora.Paradigm.Controlflow.Joint.Schemes.TUT (TUT (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype TUT ct cu cv t u t' a = TUT (t :. u :. t' := a)

instance Interpreted (TUT ct cu cv t u t') where
	type Primary (TUT ct cu cv t u t') a = t :. u :. t' := a
	run (TUT x) = x
