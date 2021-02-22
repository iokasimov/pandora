module Pandora.Paradigm.Primary.Functor.These where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))

data These e a = This a | That e | These e a

instance Covariant (These e) where
	f <$> This x = This $ f x
	_ <$> That y = That y
	f <$> These y x = These y $ f x

instance Pointable (These e) where
	point = This

instance Traversable (These e) where
	This x ->> f = This <$> f x
	That y ->> _ = point $ That y
	These y x ->> f = These y <$> f x

instance (Semigroup e, Semigroup a) => Semigroup (These e a) where
	This x + This x' = This (x + x')
	This x + That y = These y x
	This x + These y x' = These y (x + x')
	That y + This x' = These y x'
	That y + That y' = That (y + y')
	That y + These y' x = These (y + y') x
	These y x + This x' = These y (x + x')
	These y x + That y' = These (y + y') x
	These y x + These y' x' = These (y + y') (x + x')

these :: (a -> r) -> (e -> r) -> (e -> a -> r) -> These e a -> r
these f _ _ (This x) = f x
these _ g _ (That y) = g y
these _ _ h (These y x) = h y x
