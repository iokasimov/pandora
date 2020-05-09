module Pandora.Paradigm.Primary.Functor.Delta (Delta (..), type (:^:)) where

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))

data Delta a = a :^: a

type (:^:) = Delta

instance Covariant Delta where
	f <$> x :^: y = f x :^: f y

instance Pointable Delta where
	point x = x :^: x

instance Applicative Delta where
	f :^: g <*> x :^: y = f x :^: g y

instance Traversable Delta where
	x :^: y ->> f = (:^:) <$> f x <*> f y
