module Pandora.Paradigm.Junction.Schemes.TUT (TUT (..)) where

import Pandora.Core.Functor (Variant (Co), type (:.:), type (><))

data TUT ct cu t u a where
	TUT :: t' :.: u :.: t'' >< a -> TUT ct cu t u a
