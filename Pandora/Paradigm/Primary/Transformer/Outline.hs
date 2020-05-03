module Pandora.Paradigm.Primary.Transformer.Outline (Outline (..)) where

data Outline t a where
	Line :: a -> Outline t a
	Outlined :: Outline t (a -> b) -> t a -> Outline t b
