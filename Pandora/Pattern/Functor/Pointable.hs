module Pandora.Pattern.Functor.Pointable (Pointable (..)) where

import Pandora.Core.Functor (type (|->))
import Pandora.Core.Morphism ((!))
import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t => Pointable t where
	{-# MINIMAL point #-}
	point :: a |-> t

instance Pointable ((->) e) where
	point = (!)
