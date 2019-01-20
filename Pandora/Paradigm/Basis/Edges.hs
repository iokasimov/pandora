module Pandora.Paradigm.Basis.Edges where

import Pandora.Core.Morphism (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))

data Edges a = Empty | Connect a | Overlay a

instance Covariant Edges where
	f <$> Empty = Empty
	f <$> Connect x = Connect $ f x
	f <$> Overlay x = Overlay $ f x

instance Traversable Edges where
	Empty ->> f = point Empty
	Connect x ->> f = Connect <$> f x
	Overlay x ->> f = Overlay <$> f x
