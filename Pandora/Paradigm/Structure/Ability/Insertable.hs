module Pandora.Paradigm.Structure.Ability.Insertable where

infixr 2 +=

class Insertable t where
	(+=) :: a -> t a -> t a
