module Pandora.Core.Composition (type (:.:)) where

infixr 0 :.:
type (:.:) t u a = t (u a)
