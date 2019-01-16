module Paradigm.Maybe (Maybe (..)) where

import Pattern.Morphism (($))
import Pattern.Functor.Covariant (Covariant ((<$>)))

data Maybe a = Nothing | Just a

instance Covariant Maybe where
	f <$> Just x = Just $ f x
	f <$> Nothing = Nothing
