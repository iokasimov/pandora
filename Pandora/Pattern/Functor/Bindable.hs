module Pandora.Pattern.Functor.Bindable where

import Pandora.Pattern.Functor.Covariant (Covariant_)

infixr 1 =<<

{- |
> When providing a new instance, you should ensure it satisfies :
> * Interchange: t >>= f = join (f <$> t)
-}

class Covariant_ t source source => Bindable_ t source where
	(=<<) :: source a (t b) -> source (t a) (t b)
