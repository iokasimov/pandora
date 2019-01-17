module Core.Transformation (Natural, type (~>)) where

import Pattern.Functor.Covariant (Covariant)

type Natural t u = forall a . (Covariant t, Covariant u) => t a -> u a

type (~>) t u = Natural t u
