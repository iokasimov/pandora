module Pandora.Paradigm.Structure.Modification.Turnover where

import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Turnover t a = Turnover (t a)

instance Interpreted (->) (Turnover t) where
	type Primary (Turnover t) a = t a
	run ~(Turnover x) = x
	unite = Turnover
