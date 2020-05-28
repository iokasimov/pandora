module Pandora.Pattern.Functor.Extractable where

import Pandora.Core.Functor (type (<-|))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Object.Monoid (Monoid (zero))

class Covariant t => Extractable t where
	{-# MINIMAL extract #-}
	extract :: a <-| t

instance Monoid e => Extractable ((->) e) where
	extract f = f zero
