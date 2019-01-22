module Pandora.Core.Functor (Variant (..), Natural, type (~>), type (:.:)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

type Natural t u = forall a . (Covariant t, Covariant u) => t a -> u a

type (~>) t u = Natural t u

data Variant = Co | Contra

infixr 0 :.:
type (:.:) t u a = t (u a)
