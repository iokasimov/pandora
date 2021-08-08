{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Pattern.Functor.Monoidal where

import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal)

type family Unit (p :: * -> * -> *) = r | r -> p

class Semimonoidal t p source target => Monoidal t p q source target where
	unit :: p (q (Unit target) a) (t a)
