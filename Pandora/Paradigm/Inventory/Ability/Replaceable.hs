{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Replaceable where

class Replaceable i where
	type family Replacement i e r :: *
	replace:: Replacement i e r

