module Pandora.Pattern.Junction.Schemes.UTU (UTU (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))

newtype UTU ct cu t u a = UTU (u :. t u > a)

instance Composition (UTU ct cu t u) where
	type Primary (UTU ct cu t u) a = u :. t u > a
	unwrap (UTU x) = x
