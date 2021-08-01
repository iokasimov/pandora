module Pandora.Pattern.Functor.Semimonoidal where

import Pandora.Pattern.Semigroupoid (Semigroupoid)

class Semigroupoid p => Semimonoidal t p source target where
	multiply_ :: p (source (t a) (t b)) (t (target a b))
