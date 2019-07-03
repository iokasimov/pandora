module Pandora.Core.Functor (Variant (..), type (:.:), type (><)) where

data Variant = Co | Contra

infixr 1 :.:
type (:.:) t u a = t (u a)

infixr 0 ><
type (><) t a = t a
