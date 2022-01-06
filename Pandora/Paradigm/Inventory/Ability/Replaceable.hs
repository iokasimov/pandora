{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Replaceable where

class Replaceable i where
	type family Replacement i e r :: *
	replace_:: Replacement i e r

