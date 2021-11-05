module Pandora.Paradigm.Primary.Functor.Wedge where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()
import Pandora.Paradigm.Primary.Algebraic (point)

data Wedge e a = Nowhere | Here e | There a

instance Covariant (->) (->) (Wedge e) where
	_ <-|- Nowhere = Nowhere
	_ <-|- Here x = Here x
	f <-|- There x = There $ f x

instance Traversable (->) (->) (Wedge e) where
	_ <<- Nowhere = point Nowhere
	_ <<- Here x = point $ Here x
	f <<- There x = There <-|- f x

wedge :: (e -> r) -> (a -> r) -> r -> Wedge e a -> r
wedge f _ _ (Here x) = f x
wedge _ g _ (There x) = g x
wedge _ _ r Nowhere = r
