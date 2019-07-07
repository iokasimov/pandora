module Pandora.Paradigm.Junction.Schemes.TUT (TUT (..)) where

import Pandora.Core.Functor (type (:.:), type (><))

data TUT ct cu t u a where
	TUT :: t' :.: u :.: t'' >< a -> TUT ct cu t u a
