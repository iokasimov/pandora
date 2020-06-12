module Pandora.Paradigm.Primary.Functor.Delta (Delta (..), type (:^:)) where

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))

data Delta a = a :^: a

type (:^:) = Delta

instance Covariant Delta where
	f <$> x :^: y = f x :^: f y

instance Pointable Delta where
	point x = x :^: x

instance Applicative Delta where
	f :^: g <*> x :^: y = f x :^: g y

instance Distributive Delta where
	t >>- f = ((True <#>) . f <$> t) :^: ((False <#>) . f <$> t)

instance Traversable Delta where
	x :^: y ->> f = (:^:) <$> f x <*> f y

instance Representable Delta where
	type Representation Delta = Boolean
	True <#> (x :^: _) = x
	False <#> (_ :^: y) = y
	tabulate f = f True :^: f False
