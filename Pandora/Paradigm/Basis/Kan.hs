module Pandora.Paradigm.Basis.Kan (Lan (..), Ran (..)) where

import Pandora.Core.Morphism ((.))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Divariant (($))

newtype Lan (t :: * -> *) (u :: * -> *) (b :: *) (a :: *) =
	Lan { lan :: (t b -> a) -> u b }

instance Contravariant (Lan t u b) where
	f >$< Lan x = Lan $ x . (f .)

newtype Ran (t :: * -> *) (u :: * -> *) (b :: *) (a :: *) =
	Ran { ran :: (a -> t b) -> u b }

instance Covariant (Ran t u b) where
	f <$> Ran x = Ran $ x . (. f)
