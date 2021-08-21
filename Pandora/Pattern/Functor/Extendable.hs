module Pandora.Pattern.Functor.Extendable where

import Pandora.Pattern.Functor.Covariant (Covariant)

infixr 1 <<=

{- |
> When providing a new instance, you should ensure it satisfies:
> * Duplication interchange: (f -<$$>-) . (identity <<=) ≡ (identity <<=) . (f -<$>-)
> * Extension interchange: (f <<=) ≡ (f -<$>-) . (identity <<=)
-}

class Covariant source source t => Extendable t source where
	(<<=) :: source (t a) b -> source (t a) (t b)
