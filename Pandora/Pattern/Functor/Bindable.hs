module Pandora.Pattern.Functor.Bindable where

import Pandora.Pattern.Functor.Covariant (Covariant)

infixr 1 =<<

{- |
> When providing a new instance, you should ensure it satisfies :
> * Interchange: t >>= f = join (f <$> t)
-}

class Covariant t source source => Bindable t source where
	(=<<) :: source a (t b) -> source (t a) (t b)
