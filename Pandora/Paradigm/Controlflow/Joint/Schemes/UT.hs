module Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))

newtype UT ct cu t u a = UT (u :. t > a)

instance Interpreted (UT ct cu t u) where
	type Primary (UT ct cu t u) a = u :. t > a
	unwrap (UT x) = x
