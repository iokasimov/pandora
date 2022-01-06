{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Viewable where

class Viewable i where
	type family Viewing i e r :: *
	view_ :: Viewing i e r
