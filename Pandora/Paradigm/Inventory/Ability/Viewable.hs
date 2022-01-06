{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Some.Ability.Viewable where

class Viewable i where
	type family Viewing i e r :: *
	view_ :: Viewing i e r
