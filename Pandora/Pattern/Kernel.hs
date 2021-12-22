module Pandora.Pattern.Kernel (Kernel (..)) where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity))

{- |
> When providing a new instance, you should ensure it satisfies:
> * Left zero morphism: constant . f ≡ constant . g
> * Most general morphism: f . k = constant
-}

class Category m => Kernel m where
	constant :: m i a

	(.-) :: m i (m a a)
	(.-) = identity . constant
