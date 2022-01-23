module Pandora.Pattern.Functor.Semimonoidal where

import Pandora.Pattern.Semigroupoid (Semigroupoid)

class Semigroupoid m => Semimonoidal m source target t | m target -> source where
	mult :: m (source (t a) (t b)) (t (target a b))
