{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Primary.Transformer.Outline where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, (<--), (<---), (<------))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()

data Outline t a where
	Line :: a -> Outline t a
	Outlined :: t a -> Outline t (a -> b) -> Outline t b

instance Covariant (->) (->) (Outline t) where
	f <-|- Line a = Line (f a)
	f <-|- Outlined x y = Outlined x <--- (.) f <-|- y

instance Liftable (->) Outline where
	lift t = Outlined t <-- Line identity

instance Hoistable (->) Outline where
	_ /|\ Line x = Line x
	f /|\ Outlined x y = Outlined <------ f x <------ f /|\ y
