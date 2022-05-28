module Pandora.Pattern.Morphism.Tensor where

data Tensor p m q l r where
	Tensor :: m (p (t l) (t r)) (t (q l r)) -> Tensor p m q (t l) (t r) 
