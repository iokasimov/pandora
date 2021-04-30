{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Flip where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (||=)))

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)

instance Interpreted (Flip v a) where
	type Primary (Flip v a) e = v e a
	run ~(Flip x) = x
	unite = Flip

instance (forall i . Covariant (Flip v i), Bivariant v) => Bivariant (Flip v) where
	f <-> g = (g <-> f ||=)
