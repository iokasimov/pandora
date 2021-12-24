module Pandora.Paradigm.Primary.Functor.Wye where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))
import Pandora.Paradigm.Controlflow.Effect.Interpreted ((!))

data Wye a = End | Left a | Right a | Both a a

instance Covariant (->) (->) Wye where
	_ <-|- End = End
	f <-|- Left x = Left # f x
	f <-|- Right y = Right # f y
	f <-|- Both x y = Both # f x # f y

instance Semimonoidal (<--) (:*:) (:*:) Wye where
	mult = Flip ! \case
		End -> End :*: End
		Left (x :*: y) -> Left x :*: Left y
		Right (x :*: y) -> Right x :*: Right y
		Both (x :*: y) (x' :*: y') -> Both x x' :*: Both y y'
	
instance Monotonic a (Wye a) where
	reduce f r (Left x) = f x r
	reduce f r (Right x) = f x r
	reduce f r (Both x y) = f y (f x r)
	reduce _ r End = r

instance Semigroup a => Semigroup (Wye a) where
	End + x = x
	x + End = x
	Left x + Left x' = Left # x + x'
	Left x + Right y = Both x y
	Left x + Both x' y = Both # x + x' # y
	Right y + Left x = Both x y
	Right y + Right y' = Right # y + y'
	Right y + Both x y' = Both x # y + y'
	Both x y + Left x' = Both # x + x' # y
	Both x y + Right y' = Both # x # y + y'
	Both x y + Both x' y' = Both # x + x' # y + y'

instance Semigroup a => Monoid (Wye a) where
	zero = End

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
