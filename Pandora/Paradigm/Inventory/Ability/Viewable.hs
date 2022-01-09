{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Gettable where

class Gettable i where
	type family Getting i e r :: *
	get :: Getting i e r
