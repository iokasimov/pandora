module Pandora.Paradigm.Primary.Transformer.Kan (Kan (..)) where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))

data family Kan (v :: * -> k) (t :: * -> *) (u :: * -> *) b a

data instance Kan Left t u b a = Lan ((t b -> a) -> u b)

instance Contravariant (Kan Left t u b) where
	f >$< Lan x = Lan $ x . (f .)

instance Interpreted (Kan Left t u b) where
	type Primary (Kan Left t u b) a = (t b -> a) -> u b
	run (Lan x) = x

data instance Kan Right t u b a = Ran ((a -> t b) -> u b)

instance Covariant (Kan Right t u b) where
	f <$> Ran x = Ran $ x . (. f)

instance Interpreted (Kan Right t u b) where
	type Primary (Kan Right t u b) a = (a -> t b) -> u b
	run (Ran x) = x
