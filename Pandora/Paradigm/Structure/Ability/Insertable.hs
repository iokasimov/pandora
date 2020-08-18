module Pandora.Paradigm.Structure.Ability.Insertable where

class Insertable t where
	insert :: a -> t a -> t a
