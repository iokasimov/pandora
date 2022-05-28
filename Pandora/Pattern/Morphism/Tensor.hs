module Pandora.Pattern.Morphism.Tensor where

-- newtype Tensor t p q m l r = Tensor (m (p (t l) (t r)) (t (q l r))

data Tensor p q m l r where
	Tensor :: m (p (t l) (t r)) (t (q l r)) -> Tensor p q m (t l) (t r)
