module Pandora.Paradigm.Primary.Transformer.Flip where

import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Flip (t :: * -> * -> *) a e = Flip (t e a)

instance Interpreted (Flip t a) where
	type Primary (Flip t a) e = t e a
	run ~(Flip x) = x
	unite = Flip
