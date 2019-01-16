module Paradigm.Kan (Lan (..), Ran (..)) where

import Core.Morphism ((.), ($))
import Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pattern.Functor.Covariant (Covariant ((<$>)))

newtype Lan (t :: * -> *) (u :: * -> *) (b :: *) (a :: *) =
	Lan { lan :: (t b -> a) -> u b }

instance Contravariant (Lan t u b) where
	f >$< Lan x = Lan $ x . (f .)

newtype Ran (t :: * -> *) (u :: * -> *) (b :: *) (a :: *) =
	Ran { ran :: (a -> t b) -> u b }

instance Covariant (Ran t u b) where
	f <$> Ran x = Ran $ x . (. f)
