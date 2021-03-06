module Pandora.Pattern.Functor.Divisible where

import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Bivariant (Bivariant)
import Pandora.Paradigm.Primary.Functor.Product (type (:*:))

infixr 5 >*<

class Contravariant t => Divisible t where
	{-# MINIMAL (>*<) #-}
	(>*<) :: t b -> t c -> t (b :*: c)

class Bivariant v => Divisible_ t v source target where
	divide :: Bivariant v => source r (v a b) -> target (v (t a) (t b)) (t r)
