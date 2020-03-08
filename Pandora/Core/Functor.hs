module Pandora.Core.Functor where

infixr 0 :=, <-|, |->, ~>
infixr 1 .:, :.
infixr 2 ::|:., ::|.:, ::|::

-- | Parameter application
type (:=) t a = t a

-- | Functors composition
type (:.) t u a = t (u a)

-- | Flipped functors composition
type (.:) t u a = u (t a)

-- | Coalgebra's type operator
type (|->) a t = a -> t a

-- | Algebra's type operator
type (<-|) a t = t a -> a

-- | Natural transformation
type (~>) t u = forall a . t a -> u a

type (::|:.) p a b = p (p a b) b
type (::|.:) p a b = p a (p a b)
type (::|::) p a b = p (p a b) (p a b)
