module Pandora.Paradigm.Basis.Edges (Edges (..), edges) where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Divariant (($))

data Edges a = Empty | Leap a | Connect a | Overlay a

instance Covariant Edges where
	_ <$> Empty = Empty
	f <$> Connect x = Connect $ f x
	f <$> Overlay x = Overlay $ f x
	f <$> Leap x = Leap $ f x

instance Traversable Edges where
	Empty ->> _ = point Empty
	Connect x ->> f = Connect <$> f x
	Overlay x ->> f = Overlay <$> f x
	Leap x ->> f = Leap <$> f x

edges :: r -> (a -> r) -> (a -> r) -> (a -> r) -> Edges a -> r
edges r _ _ _ Empty = r
edges _ f _ _ (Connect x) = f x
edges _ _ g _ (Overlay y) = g y
edges _ _ _ h (Leap z) = h z
