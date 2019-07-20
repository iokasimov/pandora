module Pandora.Core.Functor (Variant (..), type (:.:), type (.:.), type (>)) where

data Variant = Co | Contra

infixr 1 :.:
type (:.:) t u a = t (u a)

infixr 1 .:.
type (.:.) t u a = u (t a)

infixr 0 >
type (>) t a = t a
