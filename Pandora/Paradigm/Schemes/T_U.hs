module Pandora.Paradigm.Schemes.T_U where

import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype T_U ct cu t p u a = T_U (p (t a) (u a))

instance Interpreted (T_U ct cu t p u) where
	type Primary (T_U ct cu t p u) a = p (t a) (u a)
	run ~(T_U x) = x
	unite = T_U
