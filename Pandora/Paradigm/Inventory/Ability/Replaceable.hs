{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Some.Ability.Replaceable where

class Replaceable i where
	type family Replacement i l r :: *
	replace_:: Replacement i l r

