{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Yoneda where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()

newtype Yoneda t a = Yoneda
	{ yoneda :: forall b . (a -> b) -> t b }

instance Covariant (->) (->) (Yoneda t) where
	f <-|- x = Yoneda (\k -> yoneda x (k . f))

instance Liftable (->) Yoneda where
	lift x = Yoneda (<-|- x)
