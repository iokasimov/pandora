module Pandora.Pattern.Functor.Semimonoidal where

import Pandora.Pattern.Semigroupoid (Semigroupoid)

class Semigroupoid p => Semimonoidal p source target t | p target -> source where
	mult :: p (source (t a) (t b)) (t (target a b))
