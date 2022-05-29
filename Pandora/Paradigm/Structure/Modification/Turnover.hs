{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Structure.Modification.Turnover where

import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (=#-)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Transformation.Hoistable ((/|\))
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic ((>-|-<-|-))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure))

newtype Turnover t a = Turnover (t a)

instance (Covariant m m t, Interpreted m (Turnover t)) => Covariant m m (Turnover t) where
	(<-|-) f = (=#-) ((<-|-) f)

instance Interpreted (->) (Turnover t) where
	type Primary (Turnover t) a = t a
	run ~(Turnover x) = x
	unite = Turnover

instance (Covariant (->) (->) structure, Substructure segment structure) => Substructure segment (Turnover structure) where
	type Substance segment (Turnover structure) = Substance segment structure
	substructure = (((run /|\) :*: ((unite /|\) <-|-)) >-|-<-|-) =#- substructure @segment @structure
