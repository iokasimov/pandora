module Pandora.Paradigm.Structure.Ability.Nullable where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate)

class Nullable t where
	null :: Predicate :. t := a
