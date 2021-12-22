{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic.Exponential where

import Pandora.Pattern.Betwixt (Betwixt)
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (($), (#), identity))
import Pandora.Pattern.Kernel (Kernel (constant))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))

infixr 7 -.#..-
infixr 9 %
infixl 1 &

type instance Betwixt (->) (->) = (->)

instance Semigroupoid (->) where
	f . g = \x -> f (g x)

instance Category (->) where
	identity x = x

instance Kernel (->) where
	constant x _ = x

instance Covariant (->) (->) ((->) a) where
	(<-|-) = (.)

instance Distributive (->) (->) ((->) e) where
	f -<< g = \e -> (f % e) <-|- g

instance Bindable (->) ((->) e) where
	f =<< g = \x -> f # g x # x

instance Divariant ((->)) (->) (->) (->) where
	(>->) ab cd bc = cd . bc . ab

instance Semigroup r => Semigroup (e -> r) where
	f + g = \e -> f e + g e

instance Ringoid r => Ringoid (e -> r) where
	f * g = \e -> f e * g e

type (<--) = Flip (->)

instance Contravariant (->) (->) ((<--) a) where
	f >-|- Flip g = Flip $ g . f

type (-->) = Straight (->)

instance Covariant (->) (->) ((-->) b) where
	f <-|- Straight g = Straight $ f . g

(-.#..-) :: (Covariant (->) target (v a), Semigroupoid v) => v c d -> target (v a (v b c)) (v a (v b d))
(-.#..-) f = (<-|-) (f .)

{-# INLINE (%) #-}
(%) :: (a -> b -> c) -> b -> a -> c
(%) f x y = f y x

{-# INLINE (&) #-}
(&) :: a -> (a -> b) -> b
x & f = f x

fix :: (a -> a) -> a
fix f = let x = f x in x
