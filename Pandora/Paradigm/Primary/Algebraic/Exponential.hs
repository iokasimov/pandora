{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic.Exponential where

import Pandora.Pattern.Category (Category ((.), ($), (#), identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant_ ((->$<-)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Bindable (Bindable_ ((=<<)))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))

infixr 2 !.
infixr 9 %
infixl 1 &

instance Category (->) where
	identity x = x
	f . g = \x -> f (g x)

instance Covariant ((->) a) where
	(<$>) = (.)

instance Covariant_ ((->) a) (->) (->) where
	(-<$>-) = (.)

instance Contravariant_ (Flip (->) a) (->) (->) where
	f ->$<- Flip g = Flip $ g . f

instance Applicative ((->) e) where
	(<*>) f g x = f x $ g x

instance Distributive ((->) e) (->) (->) where
	f -<< g = \e -> (f % e) -<$>- g

instance Pointable ((->) e) (->) where
	point = (!.)

instance Bindable_ ((->) e) (->) where
	f =<< g = \x -> f # g x # x

instance Representable ((->) e) where
	type Representation ((->) e) = e
	(<#>) = (identity %)
	tabulate = identity

instance Divariant ((->)) (->) (->) (->) where
	(>->) ab cd bc = cd . bc . ab

instance Semigroup r => Semigroup (e -> r) where
	f + g = \e -> f e + g e

instance Ringoid r => Ringoid (e -> r) where
	f * g = \e -> f e * g e

(-.#..-) :: (Covariant_ (v a) (->) target, Category v) => v c d -> target (v a (v b c)) (v a (v b d))
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
