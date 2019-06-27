module Pandora.Core.Functor (Variant (..), type (:.:)) where

data Variant = Co | Contra

infixr 0 :.:
type (:.:) t u a = t (u a)
