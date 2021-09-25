module Pandora.Pattern.Groupoid (Groupoid (..)) where

import Pandora.Pattern.Category (Category)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Inversion absence: inversion . inversion ≡ identity
-}

class Category m => Groupoid m where
  inversion :: m a b -> m b a
