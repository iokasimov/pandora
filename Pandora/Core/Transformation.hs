module Pandora.Core.Transformation (Natural, type (~>)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

infixr 0 ~>

type Natural t u = forall a . (Covariant t, Covariant u) => t a -> u a

type (~>) t u = Natural t u
