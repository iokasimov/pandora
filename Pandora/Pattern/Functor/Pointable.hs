module Pandora.Pattern.Functor.Pointable (Pointable (..)) where

import Pandora.Core.Functor (type (|->))
import Pandora.Core.Morphism ((!))
import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t => Pointable t where
	point :: a |-> t

instance Pointable ((->) e) where
	point = (!)
