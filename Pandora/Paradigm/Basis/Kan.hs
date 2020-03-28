module Pandora.Paradigm.Basis.Kan (Lan (..), Ran (..)) where

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype Lan t u b a = Lan ((t b -> a) -> u b)

instance Contravariant (Lan t u b) where
	f >$< Lan x = Lan $ x . (f .)

instance Interpreted (Lan t u b) where
	type Primary (Lan t u b) a = (t b -> a) -> u b
	run (Lan x) = x

newtype Ran t u b a = Ran ((a -> t b) -> u b)

instance Covariant (Ran t u b) where
	f <$> Ran x = Ran $ x . (. f)

instance Interpreted (Ran t u b) where
	type Primary (Ran t u b) a = (a -> t b) -> u b
	run (Ran x) = x
