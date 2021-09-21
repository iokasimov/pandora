module Pandora.Paradigm.Schemes.PQ_ where

import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype PQ_ p q a b = PQ_ (p a (q b a))

instance Interpreted (->) (PQ_ p q a) where
	type Primary (PQ_ p q a) b = p a (q b a)
	run ~(PQ_ x) = x
	unite = PQ_
