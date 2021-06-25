{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Flip where

import Pandora.Pattern.Functor.Covariant (Covariant_)
import Pandora.Pattern.Functor.Contravariant (Contravariant_)
import Pandora.Pattern.Functor.Bivariant (Bivariant_ ((-<->-)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (||=)))

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)

instance Interpreted (Flip v a) where
	type Primary (Flip v a) e = v e a
	run ~(Flip x) = x
	unite = Flip

instance (forall i . Covariant_ (Flip v i) (->) (->), forall i . Contravariant_ (v i) (->) (->), Bivariant_ v (->) (->) (->)) 
  	=> Bivariant_ (Flip v) (->) (->) (->) where
	f -<->- g = (g -<->- f ||=)
