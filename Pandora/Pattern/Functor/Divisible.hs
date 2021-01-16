module Pandora.Pattern.Functor.Divisible where

import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Paradigm.Primary.Functor.Product (type (:*:))

infixr 5 >*<

class Contravariant t => Divisible t where
	{-# MINIMAL (>*<) #-}

	(>*<) :: t b -> t c -> t (b :*: c)
