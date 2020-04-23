module Pandora.Paradigm.Primary.Functor.Wye (Wye (..), wye) where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Divariant (($))

data Wye a = End | Left a | Right a | Both a a

instance Covariant Wye where
	_ <$> End = End
	f <$> Left x = Left $ f x
	f <$> Right y = Right $ f y
	f <$> Both x y = Both (f x) (f y)

instance Traversable Wye where
	End ->> _ = point End
	Left x ->> f = Left <$> f x
	Right y ->> f = Right <$> f y
	Both x y ->> f = Both <$> f x <*> f y

wye :: r -> (a -> r) -> (a -> r) -> (a -> a -> r) -> Wye a -> r
wye r _ _ _ End = r
wye _ f _ _ (Left x) = f x
wye _ _ g _ (Right y) = g y
wye _ _ _ h (Both x y) = h x y
