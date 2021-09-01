module Pandora.Pattern.Morphism.Straight where

import Pandora.Core.Appliable (Appliable ((!)))

newtype Straight (v :: * -> * -> *) a e = Straight (v a e)

instance Appliable (Straight m) c b m c b where
	(!) (Straight m) = m
