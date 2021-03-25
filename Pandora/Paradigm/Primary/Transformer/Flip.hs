module Pandora.Paradigm.Primary.Transformer.Flip where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (||=)))

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)

instance Interpreted (Flip v a) where
	type Primary (Flip v a) e = v e a
	run ~(Flip x) = x
	unite = Flip

instance Bivariant v => Bivariant (Flip v) where
	f <-> g = \x -> (g <-> f) ||= x
