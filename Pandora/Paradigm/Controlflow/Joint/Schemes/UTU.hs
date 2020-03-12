module Pandora.Paradigm.Controlflow.Joint.Schemes.UTU (UTU (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype UTU ct cu t u a = UTU (u :. t u := a)

instance Interpreted (UTU ct cu t u) where
	type Primary (UTU ct cu t u) a = u :. t u := a
	run (UTU x) = x
