module Pandora.Paradigm.Schemes.P_Q_T where

import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype P_Q_T (p :: * -> * -> *) (q :: * -> * -> *) (t :: * -> *) (a :: *) (b :: *) = P_Q_T (p a (q (t b) a))

instance Interpreted (->) (P_Q_T p q t a) where
	type Primary (P_Q_T p q t a) b = (p a (q (t b) a))
	run ~(P_Q_T x) = x
	unite = P_Q_T
