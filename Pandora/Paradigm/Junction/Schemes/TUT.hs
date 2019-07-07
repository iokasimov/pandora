module Pandora.Paradigm.Junction.Schemes.TUT (TUT (..)) where

import Pandora.Core.Functor (type (:.:), type (><))

data TUT t u a where
	TUT :: t' :.: u :.: t'' >< a -> TUT t u a
