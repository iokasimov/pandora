{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Functor.Function where

import Pandora.Pattern.Category (Category ((.), ($), (#), identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))

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

instance Applicative ((->) e) where
	(<*>) f g x = f x $ g x

instance Distributive ((->) e) where
	g >>- f = \e -> (f % e) <$> g

instance Pointable ((->) e) where
	point = (!.)

instance Bindable ((->) e) where
	f >>= g = \x -> g # f x # x

instance Representable ((->) e) where
	type Representation ((->) e) = e
	(<#>) = (identity %)
	tabulate = identity

instance Divariant ((->)) where
	(>->) ab cd bc = cd . bc . ab

instance Semigroup r => Semigroup (e -> r) where
	f + g = \e -> f e + g e

instance Ringoid r => Ringoid (e -> r) where
	f * g = \e -> f e * g e

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
