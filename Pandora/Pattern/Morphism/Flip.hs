module Pandora.Pattern.Morphism.Flip where

import Pandora.Core.Appliable (Appliable ((!)))

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)

instance Appliable (Flip m) b c m c b where
	(!) (Flip m) = m
