module Pandora.Paradigm.Primary.Functor.Edges where

import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Paradigm.Algebraic.Exponential ()
import Pandora.Paradigm.Algebraic (point)

data Edges a = Empty | Leap a | Connect a | Overlay a

instance Covariant (->) (->) Edges where
	_ <-|- Empty = Empty
	f <-|- Connect x = Connect <-- f x
	f <-|- Overlay x = Overlay <-- f x
	f <-|- Leap x = Leap <-- f x

instance Traversable (->) (->) Edges where
	_ <<- Empty = point Empty
	f <<- Connect x = Connect <-|- f x
	f <<- Overlay x = Overlay <-|- f x
	f <<- Leap x = Leap <-|- f x

edges :: r -> (a -> r) -> (a -> r) -> (a -> r) -> Edges a -> r
edges r _ _ _ Empty = r
edges _ f _ _ (Connect x) = f x
edges _ _ g _ (Overlay y) = g y
edges _ _ _ h (Leap z) = h z
