module Pandora.Paradigm.Structure.Ability.Insertable where

class Insertable t where
	(+=) :: a -> t a -> t a
