module Pandora.Paradigm.Schemes.P_T where

import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype P_T p t a b = P_T (p (t a) b)

instance Interpreted (P_T p t a) where
	type Primary (P_T p t a) b = p (t a) b
	run ~(P_T x) = x
	unite = P_T
