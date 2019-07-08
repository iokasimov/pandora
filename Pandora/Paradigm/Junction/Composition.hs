module Pandora.Paradigm.Junction.Composition (Composition (..)) where

class Composition t where
	type Outline t a :: *
	composition :: t a -> Outline t a
