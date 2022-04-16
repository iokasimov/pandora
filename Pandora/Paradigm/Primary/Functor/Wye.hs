module Pandora.Paradigm.Primary.Functor.Wye where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Algebraic.Exponential (type (<--))
-- import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))

data Wye a = End | Left_ a | Right_ a | Both a a

instance Covariant (->) (->) Wye where
	_ <-|- End = End
	f <-|- Left_ x = Left_ <-- f x
	f <-|- Right_ y = Right_ <-- f y
	f <-|- Both x y = Both <-- f x <-- f y

-- instance Semimonoidal (<--) (:*:) (:*:) Wye where
-- 	mult = Flip <-- \case
-- 		End -> End :*: End
-- 		Left_ (x :*: y) -> Left_ x :*: Left_ y
-- 		Right_ (x :*: y) -> Right_ x :*: Right_ y
-- 		Both (x :*: y) (x' :*: y') -> Both x x' :*: Both y y'

instance Monotonic a (Wye a) where
	reduce f r (Left_ x) = f x r
	reduce f r (Right_ x) = f x r
	reduce f r (Both x y) = f y <-- f x r
	reduce _ r End = r

instance Semigroup a => Semigroup (Wye a) where
	End + x = x
	x + End = x
	Left_ x + Left_ x' = Left_ <-- x + x'
	Left_ x + Right_ y = Both <-- x <-- y
	Left_ x + Both x' y = Both <-- x + x' <-- y
	Right_ y + Left_ x = Both <-- x <-- y
	Right_ y + Right_ y' = Right_ <-- y + y'
	Right_ y + Both x y' = Both <-- x <-- y + y'
	Both x y + Left_ x' = Both <-- x + x' <-- y
	Both x y + Right_ y' = Both <-- x <-- y + y'
	Both x y + Both x' y' = Both <-- x + x' <-- y + y'

instance Semigroup a => Monoid (Wye a) where
	zero = End

wye :: r -> (a -> r) -> (a -> r) -> (a -> a -> r) -> Wye a -> r
wye r _ _ _ End = r
wye _ f _ _ (Left_ x) = f x
wye _ _ g _ (Right_ y) = g y
wye _ _ _ h (Both x y) = h x y

swop :: Wye ~> Wye
swop End = End
swop (Both l r) = Both r l
swop (Left_ l) = Right_ l
swop (Right_ r) = Left_ r
