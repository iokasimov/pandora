module Pandora.Pattern.Junction.Schemes.TU (TU (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Junction.Interpreted (Interpreted (Primary, unwrap))

newtype TU ct cu t u a = TU (t :. u > a)

instance Interpreted (TU ct cu t u) where
	type Primary (TU ct cu t u) a = t :. u > a
	unwrap (TU x) = x
