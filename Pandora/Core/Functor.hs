module Pandora.Core.Functor where

infixl 9 <
infixl 8 <<
infixl 7 <<<
infixl 6 <<<<
infixl 5 <<<<<
infixl 4 <<<<<<
infixl 3 <<<<<<<
infixl 2 <<<<<<<<
infixl 1 <<<<<<<<<

infixr 0 >
infixr 0 <:=, :=>, :=:=>, ~>
infixr 1 .:, :.
infixr 2 ::|:., ::|.:, ::|::
infixr 9 :::

-- | Arguments consuming
type (<) t a = t a
type (<<) t a = t a
type (<<<) t a = t a
type (<<<<) t a = t a
type (<<<<<) t a = t a
type (<<<<<<) t a = t a
type (<<<<<<<) t a = t a
type (<<<<<<<<) t a = t a
type (<<<<<<<<<) t a = t a

-- | Type application
type (>) t a = t a

-- | Functors composition
type (:.) t u a = t (u a)

-- | Flipped functors composition
type (.:) t u a = u (t a)

-- | Coalgebra's type operator
type (:=>) a t = a -> t a

type (:=:=>) a t = a -> t a -> t a

-- | Algebra's type operator
type (<:=) a t = t a -> a

-- | Natural transformation
type t ~> u = forall a . t a -> u a

type t ~~> u = forall a b . t a b -> u a b

type (::|:.) p a b = p (p a b) b
type (::|.:) p a b = p a (p a b)
type (::|::) p a b = p (p a b) (p a b)

-- Type operator for anything higher kinded
data (:::) t u :: k -> k' -> *
