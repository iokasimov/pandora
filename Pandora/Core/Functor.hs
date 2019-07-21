module Pandora.Core.Functor where

data Variant = Co | Contra

infixr 1 :.
type (:.) t u a = t (u a)

infixr 1 .:
type (.:) t u a = u (t a)

infixr 0 >
type (>) t a = t a

infixr 2 ::|:., ::|.:, ::|::
type (::|:.) p a b = p (p a b) b
type (::|.:) p a b = p a (p a b)
type (::|::) p a b = p (p a b) (p a b)
