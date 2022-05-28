module Pandora.Pattern.Morphism.Tensor where

newtype Tensor t p q m l r = Tensor (m (p (t l) (t r)) (t (q l r)))
