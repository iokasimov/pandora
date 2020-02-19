module Pandora.Pattern.Junction.Schemes.TUV (TUV (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Junction.Interpreted (Interpreted (Primary, unwrap))

newtype TUV ct cu cv t u v a = TUV (t :. u :. v > a)

instance Interpreted (TUV ct cu cv t u v) where
	type Primary (TUV ct cu cv t u v) a = t :. u :. v > a
	unwrap (TUV x) = x
