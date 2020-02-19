module Pandora.Pattern.Junction.Schemes.TUVW (TUVW (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Junction.Interpreted (Interpreted (Primary, unwrap))

newtype TUVW ct cu cv cw t u v w a = TUVW (t :. u :. v :. w > a)

instance Interpreted (TUVW ct cu cv cw t u v w) where
	type Primary (TUVW ct cu cv cw t u v w) a = t :. u :. v :. w > a
	unwrap (TUVW x) = x
