module Pandora.Pattern.Functor.Semimonoidal where

import Pandora.Pattern.Semigroupoid (Semigroupoid)
import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$)))

class Semigroupoid p => Semimonoidal t p source target where
	multiply_ :: p (source (t a) (t b)) (t (target a b))
