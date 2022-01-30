{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Structure.Modification.Turnover where

import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Transformer.Hoistable ((/|\))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic ((>-|-<-|-))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (=#-)))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure))

newtype Turnover t a = Turnover (t a)

instance (Covariant m m t, Interpreted m (Turnover t)) => Covariant m m (Turnover t) where
	(<-|-) f = (=#-) ((<-|-) f)

instance Interpreted (->) (Turnover t) where
	type Primary (Turnover t) a = t a
	run ~(Turnover x) = x
	unite = Turnover

instance (Covariant (->) (->) structure, Substructure segment structure) => Substructure segment (Turnover structure) where
	type Available segment (Turnover structure) = Available segment structure
	type Substance segment (Turnover structure) = Substance segment structure
	substructure = ((run /|\) :*: ((unite /|\) <-|-) >-|-<-|-) =#- substructure @segment @structure
