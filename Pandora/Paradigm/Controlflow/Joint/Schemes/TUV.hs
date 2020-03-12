module Pandora.Paradigm.Controlflow.Joint.Schemes.TUV (TUV (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype TUV ct cu cv t u v a = TUV (t :. u :. v := a)

instance Interpreted (TUV ct cu cv t u v) where
	type Primary (TUV ct cu cv t u v) a = t :. u :. v := a
	run (TUV x) = x
