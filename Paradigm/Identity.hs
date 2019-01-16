module Paradigm.Identity (Identity (..)) where

import Core.Morphism (($))
import Pattern.Functor.Covariant (Covariant ((<$>)))

newtype Identity a = Identity a

instance Covariant Identity where
	f <$> Identity x = Identity $ f x
