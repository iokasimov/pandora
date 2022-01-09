{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Settable where

class Settable i where
	type family Setting i e r :: *
	set:: Setting i e r

