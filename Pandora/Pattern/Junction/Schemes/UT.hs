module Pandora.Pattern.Junction.Schemes.UT (UT (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))

newtype UT ct cu t u a = UT (u :. t > a)

instance Composition (UT ct cu t u) where
	type Primary (UT ct cu t u) a = u :. t > a
	unwrap (UT x) = x
