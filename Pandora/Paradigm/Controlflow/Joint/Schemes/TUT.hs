module Pandora.Paradigm.Controlflow.Joint.Schemes.TUT (TUT (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype TUT ct cu cv t u v a = TUT (t :. u :. v := a)

instance Interpreted (TUT ct cu cv t u v) where
	type Primary (TUT ct cu cv t u v) a = t :. u :. v := a
	run (TUT x) = x
