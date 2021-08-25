{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic.Exponential where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (($), (#), identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((->$<-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))

infixr 2 !.
infixr 7 -.#..-
infixr 9 %
infixl 1 &

instance Semigroupoid (->) where
	f . g = \x -> f (g x)

instance Category (->) where
	identity x = x

instance Covariant (->) (->) ((->) a) where
	(-<$>-) = (.)

instance Distributive (->) (->) ((->) e) where
	f -<< g = \e -> (f % e) -<$>- g

instance Bindable (->) ((->) e) where
	f =<< g = \x -> f # g x # x

instance Divariant ((->)) (->) (->) (->) where
	(>->) ab cd bc = cd . bc . ab

instance Semigroup r => Semigroup (e -> r) where
	f + g = \e -> f e + g e

instance Ringoid r => Ringoid (e -> r) where
	f * g = \e -> f e * g e

type (<--) = Flip (->)

instance Semigroupoid (<--) where
	Flip f . Flip g = Flip $ \x -> g (f x)

instance Category (<--) where
	identity = Flip identity

instance Contravariant (->) (->) ((<--) a) where
	f ->$<- Flip g = Flip $ g . f

(-.#..-) :: (Covariant (->) target (v a), Semigroupoid v) => v c d -> target (v a (v b c)) (v a (v b d))
(-.#..-) f = (-<$>-) (f .)

{-# INLINE (!.) #-}
(!.) :: a -> b -> a
x !. _ = x

{-# INLINE (!..) #-}
(!..) :: a -> b -> c -> a
(!..) x _ _ = x

{-# INLINE (!...) #-}
(!...) :: a -> b -> c -> d -> a
(!...) x _ _ _ = x

{-# INLINE (%) #-}
(%) :: (a -> b -> c) -> b -> a -> c
(%) f x y = f y x

{-# INLINE (&) #-}
(&) :: a -> (a -> b) -> b
x & f = f x

fix :: (a -> a) -> a
fix f = let x = f x in x
