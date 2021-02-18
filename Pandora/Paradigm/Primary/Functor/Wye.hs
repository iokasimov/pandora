module Pandora.Paradigm.Primary.Functor.Wye where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))

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

instance Monotonic a (Wye a) where
	reduce f r (Left x) = f x r
	reduce f r (Right x) = f x r
	reduce f r (Both x y) = f y (f x r)
	reduce _ r End = r

wye :: r -> (a -> r) -> (a -> r) -> (a -> a -> r) -> Wye a -> r
wye r _ _ _ End = r
wye _ f _ _ (Left x) = f x
wye _ _ g _ (Right y) = g y
wye _ _ _ h (Both x y) = h x y

swop :: Wye ~> Wye
swop End = End
swop (Both l r) = Both r l
swop (Left l) = Right l
swop (Right r) = Left r
