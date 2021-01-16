module Pandora.Pattern.Functor.Divisible where

import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Paradigm.Primary.Functor.Product (type (:*:))

infixl 4 >*<

class Contravariant t => Divisible t where
	{-# MINIMAL (>*<) #-}

	(>*<) :: (a -> b :*: c) -> t b -> t c -> t a
