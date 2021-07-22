module Pandora.Pattern.Functor.Extendable where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_)

infixr 1 -<<=-

{- |
> When providing a new instance, you should ensure it satisfies:
> * Duplication interchange: (f -<$$>-) . (identity -<<=-) ≡ (identity -<<=-) . (f -<$>-)
> * Extension interchange: (f -<<=-) ≡ (f -<$>-) . (identity -<<=-)
-}

class Covariant_ t source source => Extendable_ t source where
	(-<<=-) :: source (t a) b -> source (t a) (t b)
