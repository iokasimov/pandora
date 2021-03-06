module Pandora.Paradigm.Structure.Ability.Insertable where

import Pandora.Core.Functor (type (:=:=>))

infixr 2 +=

class Insertable t where
	(+=) :: a :=:=> t
