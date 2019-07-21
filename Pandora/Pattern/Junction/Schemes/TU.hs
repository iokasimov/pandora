module Pandora.Pattern.Junction.Schemes.TU (TU (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))

newtype TU ct cu t u a = TU (t :. u > a)

instance Composition (TU ct cu t u) where
	type Primary (TU ct cu t u) a = t :. u > a
	unwrap (TU x) = x
