module Pandora.Paradigm.Schemes.U_T where

import Pandora.Core.Interpreted (Interpreted (Primary, run, unite))

newtype U_T ct cu t p u a = U_T (p (u a) (t a))

instance Interpreted (->) (U_T ct cu t p u) where
	type Primary (U_T ct cu t p u) a = p (u a) (t a)
	run ~(U_T x) = x
	unite = U_T
