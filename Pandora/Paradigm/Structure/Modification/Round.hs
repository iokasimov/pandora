module Pandora.Paradigm.Structure.Modification.Round where

import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Round t a = Round (t a)

instance Interpreted (->) (Round t) where
	type Primary (Round t) a = t a
	run ~(Round x) = x
	unite = Round
